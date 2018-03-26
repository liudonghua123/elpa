;;; gnus-article-notes.el --- Attach notes to messages in Gnus   -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2017_12_11
;; Keywords: news registry
;; Version: 0.1
;; Package-Requires: ()


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This simple package allows to attach text notes to articles in
;; Gnus.  This is actually just a trivial convenience wrapper around
;; `gnus-registry-set-id-key' and `gnus-registry-get-id-key'.
;;
;; For something less simplistic see the Gnorb package in Gnu Elpa.
;; It can save notes in org files, track discussions, and much more.
;;
;;
;; Usage
;; =====
;;
;; The main command is `gnus-article-notes-set-note' bound to "@" in
;; the summary keymap.
;;
;; If the current article has not yet an attached note, hit @ to add
;; one.  The article is also flagged with an "@" to indicate that a
;; note has been attached.
;;
;; When an article has already an attached note, "@" displays the note
;; in the echo area, and hitting "@" again let's you edit the note.
;; "@" with a prefix argument 0 deletes the note after confirmation.
;; "@" with any other prefix arg also reads in a note text but using a
;; pop-up buffer instead of the minibuffer making editing multi-line
;; notes more convenient.
;;
;;
;; Setup
;; =====
;;
;; Somewhere in your initialization you need to enable the Gnus
;; registry (where this package saves your notes), load this file, and
;; make the key binding:
;;
;;   (gnus-registry-initialize)
;;   (require 'gnus-article-notes)
;;   (add-hook
;;    'gnus-summary-mode-hook
;;    (defun my-gnus-summary-mode-hook-bind-key-for-article-notes ()
;;      (define-key gnus-summary-mode-map [?@] #'gnus-article-notes-set-note)))
;;
;; It is a good idea to read about what enabling the registry means if
;; you haven't yet used it: (info "(gnus) The Gnus Registry").  It is
;; easy stuff.  You may want to limit how much data Gnus stores in the
;; registry to avoid delays when saving (it stores a lot by default).
;; I do (setq gnus-registry-max-entries 2000).  Note that pruning a
;; full registry will never delete notes unless you change
;; `gnus-registry-extra-entries-precious' to not contain `mark'.
;; Loading this package adds a "Note" named custom mark to
;; `gnus-registry-marks' (by default).
;;
;; To see the "@" marker for messages with attached notes in the
;; summary buffer, you also want something like
;;
;;   (defalias 'gnus-user-format-function-M
;;             'gnus-registry-article-marks-to-chars)
;;
;; which allows you to use "%uM" (or better with a padding like in
;; "%2uM") in `gnus-summary-line-format' to show registry marks - see
;; (info "(gnus) Store custom flags and keywords") for details.
;;
;; Finally you may also want to look at the few customizable options
;; defined in this file.



;;; Code:



(eval-when-compile (require 'subr-x))
(require 'gnus)
(require 'gnus-registry)

(defvar gnus-article-notes-registry-field 'Note)
(defvar gnus-article-notes-marker-char ?@)
(defvar gnus-article-notes-auto-tick nil)

(defvar gnus-article-notes-show-in-summary t)

(defun gnus-article-notes-registry-delete-id-key (id key)
  (let* ((db gnus-registry-db)
         (entry (gnus-registry-get-or-make-entry id)))
    (registry-delete db (list id) nil)
    (setq entry (assq-delete-all key entry))
    (gnus-registry-insert db id entry)
    entry))

(with-eval-after-load 'gnus-registry
  (add-to-list 'gnus-registry-marks
               `(,gnus-article-notes-registry-field :char ,gnus-article-notes-marker-char :image nil)))

(defvar gnus-article-notes-popup-window-action '())

;; We could make the major mode customizable...
(defun gnus-article-notes-read-string-with-buffer (&optional initial-input keymap comment)
  (cl-callf or comment ";; Hit C-c C-c when done\n\n") ;FIXME: add key to abort
  (save-window-excursion
    (with-temp-buffer
      (let ((win (display-buffer (current-buffer) gnus-article-notes-popup-window-action)))
        (select-window win)
        (insert comment)
        (when initial-input (insert initial-input))
        (set-window-point win (point-max))
        (use-local-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map (or keymap text-mode-map))
                         (define-key map [(control ?c) (control ?c)] #'exit-recursive-edit)
                         map))
        (recursive-edit)
        (string-trim
         (replace-regexp-in-string
          (concat "\\`" (regexp-quote comment)) ""
          (buffer-string)))))))

(defun gnus-article-notes-set-note (id new-content)
  (if (not new-content)
      (gnus-article-notes-registry-delete-id-key id gnus-article-notes-registry-field)
    (gnus-registry-set-id-key id gnus-article-notes-registry-field new-content)))

(defun gnus-article-notes-display-or-set-note (article id &optional content)
  "Doc..."
  (interactive
   (let* ((articles (gnus-summary-work-articles nil))
          (article (if (cdr articles) (user-error "Cannot operate on multiple articles")
                     (car articles)))
          (id (mail-header-id (gnus-summary-article-header article)))
          (current-content (gnus-registry-get-id-key id gnus-article-notes-registry-field)))
     (list article
           id
           (if (or (eq this-command last-command) (not current-content) current-prefix-arg)
               (let ((new-content
                      (if current-prefix-arg
                          (if (eq 0 (prefix-numeric-value current-prefix-arg))
                              (if (yes-or-no-p "Delete this note? ")
                                  nil
                                (user-error "Abort"))
                            (gnus-article-notes-read-string-with-buffer current-content))
                        (read-string "New note: " current-content))))
                 (if (equal "" new-content) nil new-content))
             `(display . ,current-content)))))
  (pcase content
    (`(display . ,content) (message "%s" content))
    (_ (when (and content gnus-article-notes-auto-tick) (gnus-summary-tick-article-forward 1))
       (gnus-article-notes-set-note id content)
       (gnus-registry--set/remove-mark 'Note (not content) (list article)))))

(defun gnus-article-notes-get-additional-articles (group-name)
  (delq nil
        (mapcar (lambda (id) (cdr (gnus-request-head id group-name)))
                (cl-loop for key being the hash-keys of (oref gnus-registry-db data)
                         using (hash-values v)
                         when (assoc gnus-article-notes-registry-field v)
                         collect key))))


(defun gnus-articles-notes-alter-articles-to-read-function (f group-name article-list)
  (let ((others (funcall f group-name article-list)))
    (if gnus-article-notes-show-in-summary
        (cl-union (gnus-article-notes-get-additional-articles group-name)
                  others)
      others)))

(add-function :around gnus-alter-articles-to-read-function
              #'gnus-articles-notes-alter-articles-to-read-function)



(provide 'gnus-article-notes)
;;; gnus-article-notes.el ends here
