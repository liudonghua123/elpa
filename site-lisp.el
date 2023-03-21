;;; site-lisp.el --- Manage site-lisp directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022, 2023  Philip Kaludercic

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <~pkal/public-inbox@lists.sr.ht>
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.0
;; URL: https://git.sr.ht/~pkal/site-lisp
;; Keywords: lisp, local

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

;; The following code manages local Lisp code, that might not be part
;; of a package.  For regular use, create a "site-lisp" directory next
;; to "init.el", and create a file subdirectory for every script you
;; wish to have loaded.
;;
;; Use `site-lisp-reload' after adding a new script to avoid
;; restarting Emacs.

;;; Code:

(require 'seq)

(defgroup site-lisp ()
  "Manage site-lisp directories."
  :group 'lisp)

(defcustom site-lisp-autoload-file ".auto-site.el"
  "Name of file to store autoload forms in."
  :type 'string)

(defmacro site-lisp-generate-autoloads (dir file)
  "Generate autoloads for DIR as appropriate for the current version.
The result should be stored in FILE."
  (cond
   ((version<= "29" emacs-version)
    `(loaddefs-generate ,dir ,file))
   ((version<= "28" emacs-version)
    `(make-directory-autoloads ,dir ,file))
   (`(let ((generated-autoload-file ,file))
       (update-directory-autoloads ,dir)))))

(defvar generated-autoload-file)

;;;###autoload
(defun site-lisp-prepare (dir)
  "Byte-compile, prepare autoloads and note each directory in DIR.
If DIR is a list, the function will be applied to every element
of the list."
  (interactive "DPrepare: ")
  (let ((backup-inhibited t)
        (autoload-file (expand-file-name site-lisp-autoload-file dir)))
    (dolist (dir (cons dir (directory-files dir t "^[^.]")))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (site-lisp-generate-autoloads dir autoload-file)
        (byte-recompile-directory dir)))
    (load autoload-file nil t)))

(defun site-lisp-unprepare (dir)
  "Remove every directory in DIR from `load-path'.
If DIR is a list, the function will be applied to every element
of the list."
  (interactive "DUnprepare: ")
  (dolist (sub-dir (directory-files dir t "^[^.]"))
    (when (seq-find (apply-partially #'file-equal-p sub-dir)
                    load-path)
      (when (memq dir load-path)
        (setq load-path (delq dir load-path))
        (dolist (ent (alist-get sub-dir load-history
                                nil nil #'file-equal-p))
          (when (eq (car-safe ent) 'provide)
            (with-demoted-errors "Error while unloading: %S"
              (unload-feature (cdr ent)))))))))

;;;###autoload
(defcustom site-lisp-directory
  (locate-user-emacs-file "site-lisp")
  "Directory use for site-local Lisp code.
If this directory doesn't exist, nothing is done."
  :set (lambda (var val)
         (when (bound-and-true-p site-lisp-directory)
           (site-lisp-unprepare site-lisp-directory))
         (site-lisp-prepare val)
         (custom-set-default var val))
  :type 'directory)

(defun site-lisp-reload ()
  "Reload the contents of `site-lisp-directory'."
  (interactive)
  (unless (file-directory-p site-lisp-directory)
    (user-error "%s is not an existing directory"
                site-lisp-directory))
  (site-lisp-prepare site-lisp-directory))

;;;###autoload
(defalias 'site-lisp-initialise #'site-lisp-reload)

(add-hook 'after-init-hook #'site-lisp-reload)

(provide 'site-lisp)
;;; site-lisp.el ends here
