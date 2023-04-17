;;; calibre-library.el --- View and interact with the Calibre library -*- lexical-binding:t -*-

;; Copyright (C) 2023  Kjartan Oli Agustsson

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; View the contents of a Library, add new books, remove books, and
;; open books in Emacs.

;;; Code:
(require 'calibre-book)

(defconst calibre-library-buffer "*Library*")

(defun calibre-library--refresh (&optional force)
  "Refresh the contents of the library buffer.
If FORCE is non-nil fetch book data from the database."
  (let* ((buffer (get-buffer calibre-library-buffer)))
      (with-current-buffer buffer
        (setf tabulated-list-entries
              (mapcar #'calibre-book--print-info
                      (calibre--books force)))
        (tabulated-list-print))))

;;;###autoload
(defun calibre-library-add-book (file)
  "Add FILE to the Calibre library."
  (interactive "f")
  (calibre-library-add-books (list file)))

(defun calibre-library-add-books (files)
  "Add FILES to the Calibre library."
  (calibre-library--execute `("add" ,@(mapcar #'expand-file-name files))))

;;;###autoload
(defun calibre-dired-add ()
  "Add marked files to the Calibre library."
    (interactive)
    (if (derived-mode-p 'dired-mode)
        (calibre-library-add-books (dired-get-marked-files))))

(defun calibre-remove-books (books)
  "Remove BOOKS from the Calibre library."
  (let ((ids (mapcar #'int-to-string (mapcar #'calibre-book-id books))))
    (calibre-library--execute `("remove" ,(string-join ids ",")))))

(defun calibre-library--process-sentinel (_ event)
  "Process filter for Calibre library operations.
EVENT is the process event, see Info node
`(elisp)Sentinels'"
  (if (string= event "finished\n")
      (if (get-buffer calibre-library-buffer)
          (calibre-library--refresh))
    (error "Calibre process failed %S" event)))

(cl-defun calibre-library--execute (args &optional (sentinel #'calibre-library--process-sentinel))
  "Execute calibredb with arguments ARGS.
ARGS should be a list of strings.  SENTINEL is a process sentinel to install."
  (if (not (executable-find calibre-calibredb-executable))
      (error "Could not find calibredb")
    (make-process
     :name "calibre"
     :command `("calibredb" "--with-library" ,calibre-library-dir ,@args)
     :sentinel sentinel)))

(defun calibre-library-mark-remove (&optional _num)
  "Mark a book for removal and move to the next line."
  (interactive "p" calibre-library-mode)
  (tabulated-list-put-tag "D" t))

(defun calibre-library-mark-unmark (&optional _num)
  "Clear any marks on a book and move to the next line."
  (interactive "p" calibre-library-mode)
  (tabulated-list-put-tag " " t))

(defun calibre-library-execute ()
  "Performed marked Library actions."
  (interactive nil calibre-library-mode)
  (let (remove-list mark)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setf mark (char-after))
        (cl-case mark
          (?\D (push (tabulated-list-get-id) remove-list)))
        (forward-line)))
    (when remove-list (calibre-remove-books remove-list)))
  (calibre--books t)
  (calibre-library--refresh))

(defun calibre-library-open-book (book)
  "Open BOOK in its preferred format."
  (interactive (list (tabulated-list-get-id)) calibre-library-mode)
  (find-file (calibre-book--file book (calibre-book--pick-format book))))

(defun calibre-library--header-format ()
  (vconcat
   (mapcar (lambda (x)
             (let ((column (car x))
                   (width (cdr x)))
               (cl-case column
                 (id `("ID" ,width (lambda (a b)
                                     (< (calibre-book-id (car a))
                                        (calibre-book-id (car b))))))
                 (title `("Title" ,width))
                 (authors `("Author(s)" ,width))
                 (publishers `("Publisher(s)" ,width))
                 (series `("Series" ,width (lambda (a b)
                                             (calibre-book-sort-by-series (car a) (car b)))))
                 (series-index `("#" ,width))
                 (tags `("Tags" ,width))
                 (formats `("Formats" ,width)))))
           calibre-library-columns)))

(defvar-keymap calibre-library-mode-map
  :doc "Local keymap for Calibre Library buffers."
  :parent tabulated-list-mode-map
  "d" #'calibre-library-mark-remove
  "u" #'calibre-library-mark-unmark
  "x" #'calibre-library-execute
  "a" #'calibre-library-add-book
  "v" #'calibre-view-book
  "e" #'calibre-edit-book
  "RET" #'calibre-library-open-book)

(define-derived-mode calibre-library-mode tabulated-list-mode
  (setf tabulated-list-padding 2
        tabulated-list-format (calibre-library--header-format))
  (tabulated-list-init-header))

;;;###autoload
(defun calibre-library ()
  "List all books in Calibre Library `calibrary-dir'."
  (interactive)
  (let ((buffer (get-buffer-create calibre-library-buffer)))
    (with-current-buffer buffer
      (calibre-library-mode)
      (calibre-library--refresh t)
      (display-buffer buffer))))

(provide 'calibre-library)
;;; calibre-library.el ends here
