;;; fannypack.el --- They say it's back in style -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Theodor Thornhill

;; Author: Theodor Thornhill <theo@thornhill.no>
;; Keywords: tools languages
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (project "0.8.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A fannypack is a persisted list of paths that are considered important for
;; the ongoing work.  Thus, we rely on git branch names to distinguish between
;; fannypacks.  The use case is as follows:
;;
;; * Create a branch and start the ongoing work.
;; * Discover what files are important, place them in a fannypack.
;; * When exploring the code base in the current project, you can more easily now
;;   jump to the important files, thus saving time.
;; * Realize you need to work on a different branch - switch to it.
;;   Now the fannypack is automatically scoped to the new branch.
;;   If there are files there, jump to them.
;;
;;  So to clarify, fannypack persists a set of files residing under a project as
;;  defined by `project'.  In addition, we use git branches to delimit between
;;  different sets of files.

;;; Code:

(require 'vc-git)
(require 'project)

(defgroup fannypack nil
  "You cannot leave your house without your fannypack."
  :group 'tools)

(defcustom fannypack-directory
  (concat user-emacs-directory ".local/fannypack/")
  "Where the fannypacks will be saved."
  :type 'string)

(defvar fannypack--default-directory nil
  "Default directory override.")

(defun fannypack--ensure-directory ()
  "Create `fannypack-directory' if it doesn't exist, else noop."
  (unless (file-exists-p (expand-file-name fannypack-directory))
    (make-directory (file-truename fannypack-directory) t)))

(defun fannypack--name ()
  (cl-flet ((normalize (file-name)
              (string-replace "/" "---" file-name)))
    (let ((default-directory (or fannypack--default-directory
                                 (project-root (project-current t)))))
      (file-truename
       (concat fannypack-directory
               (concat
                (normalize default-directory)
                "#"
                (normalize (car (vc-git-branches)))))))))

(defun fannypack--read ()
  "Read files from the current `fannypack-default-directory'.
This is a file named after the path of the directory it refers
to.  There can be several of those files, appended with the
current branch name.  If the file can be found, we read that file
into lisp data."
  (let ((filename (fannypack--name)))
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer))))))

(defun fannypack--persist (fannypack)
  "Persist the current fannypack into storage.
When updating the fannypack, we try to persist it to disk, so
that it can easily be retrieved at a later time."
  (let ((filename (fannypack--name)))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp fannypack (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun fannypack--keep-sort-order (completions)
  ;; Small hack to avoid the default sorting order to apply, which is
  ;; alphabetically.
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun fannypack--completing-read (prompt fannypack)
  "Read the current fannypack, and show a completion selection.
We do make sure we keep the order the files are stored in, so
that `fannypack-promote' and `fannypack-demote' can do its
thing."
  (let ((default (caar fannypack)))
    (if fannypack
        (completing-read
         (format prompt (file-name-nondirectory default))
         (fannypack--keep-sort-order fannypack) nil t nil nil default)
      (user-error "Fannypack is empty!"))))

;;;###autoload
(defun fannypack-place ()
  "Place the current file into the fannypack.
It is placed into a fannypack controlled by the git branch."
  (interactive)
  (fannypack--ensure-directory)
  (let ((fannypack (fannypack--read))
        (entry (list buffer-file-name)))
    (unless (member entry fannypack)
      (setq fannypack (append fannypack (list entry)))
      (fannypack--persist fannypack)
      (message "Placed in fannypack!"))))

;;;###autoload
(defun fannypack-pick ()
  "Choose a file from the current fannypack.
After selection, we jump to the chosen file."
  (interactive)
  (find-file
   (fannypack--completing-read
    "Fannypack pick [%s]: "
    (remove (list buffer-file-name)
            (fannypack--read)))))

;;;###autoload
(defun fannypack-feeling-lucky (fannypack)
  "Jump to the file currently at the top of the fannypack."
  (interactive
   (list (caar (remove (list buffer-file-name) (fannypack--read)))))
  (if fannypack
      (find-file fannypack)
    (user-error "Fannypack is empty!")))

;;;###autoload
(defun fannypack-burn ()
  "Delete one fannypack.
This is limited to the one on the current branch."
  (interactive)
  (when (y-or-n-p "Burn this fannypack?")
    (fannypack--persist nil)))

;;;###autoload
(defun fannypack-drop (fannypack)
  "Remove one file from the fannypack."
  (interactive (list (fannypack--read)))
  (when-let ((entry
              (list
               (fannypack--completing-read
                "Fannypack drop [%s]: "
                fannypack))))
    (setq fannypack (remove entry fannypack))
    (fannypack--persist fannypack)
    (message "Dropped %s from fannypack"
             (file-name-nondirectory (car entry)))))

;;;###autoload
(defun fannypack-promote (fannypack)
  "Lift a file to the top of the current fannypack."
  (interactive (list (fannypack--read)))
  (let ((entry
         (list
          (fannypack--completing-read
           "Fannypack promote [%s]: "
           fannypack))))
    (setq fannypack (remove entry fannypack))
    (push entry fannypack)
    (fannypack--persist fannypack)
    (message "Promoted %s to top in fannypack"
             (file-name-nondirectory (car entry)))))

;;;###autoload
(defun fannypack-demote (fannypack)
  "Push a file to the bottom of the current fannypack."
  (interactive (list (fannypack--read)))
  (let ((entry
         (list
          (fannypack--completing-read
           "Fannypack demote [%s]: "
           fannypack))))
    (setq fannypack (remove entry fannypack))
    (setq fannypack (append fannypack (list entry)))
    (fannypack--persist fannypack)
    (message "Demoted %s to bottom in fannypack"
             (file-name-nondirectory (car entry)))))

;;;###autoload
(defun fannypack-default-directory (dir)
  "Set the directory DIR as the default `fannypack-default-directory'."
  (interactive "P")
  (setq fannypack--default-directory
        (cond
         ((equal dir '(4))
          (project-root (project-current t)))
         ((equal dir '(16))
          (read-file-name "Default fannypack: " nil default-directory 'mustmatch)))))

(provide 'fannypack)
;;; fannypack.el ends here
