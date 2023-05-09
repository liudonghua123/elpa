;;; calibre-edit.el --- Edit Book metadata  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kjartan Oli Agustsson

;; This file is part of calibre.el.

;; calibre.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; calibre.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with calibre.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'eieio-custom)
(require 'calibre-core)
(require 'calibre-util)
(require 'calibre-exec)

(defvar calibre-edit--edited-books nil
  "A list containing the original copies of edited books.")

(defun calibre-edit-book (book)
  "Edit the metadata of BOOK."
  (interactive (list (tabulated-list-get-id)) calibre-library-mode)
  (unless (calibre-util-find-book book calibre-edit--edited-books)
    (push (clone book) calibre-edit--edited-books))
  (eieio-customize-object book))

(cl-defmethod eieio-done-customizing ((book calibre-book))
  (calibre-library--refresh)
  (with-current-buffer (get-buffer calibre-library-buffer)
    (calibre-library--find-book book)
    (tabulated-list-put-tag "M")))

(defun calibre-edit-revert (book)
  "Undo any edits performed to BOOK in this session."
  (let ((compare-ids (lambda (b)
                            (= (calibre-book-id book)
                               (calibre-book-id b)))))
    (setf calibre--books
        (cl-substitute-if (calibre-util-find-book book calibre-edit--edited-books)
                          compare-ids
                          calibre--books))
    (calibre-library--refresh)))

(defun calibre-edit--find-original (book)
  "Return BOOK absent any modifications performed in this session."
  (calibre-util-find-book book calibre-edit--edited-books))

(defun calibre-edit--different-fields (a b)
  "Return a list of slot names whose values differ in A and B."
  (let (diff)
    (dolist (descriptor (eieio-class-slots 'calibre-book))
      (let* ((slot-name (eieio-slot-descriptor-name descriptor))
             (a (slot-value a slot-name))
             (b (slot-value b slot-name)))
        (unless (equal a b)
          (push slot-name diff))))
    diff))

(defun calibre-util-uglify-field-name (field)
  "Return FIELD replacing - with -."
  (string-join (split-string (symbol-name field) "-") "_"))

(defun calibre-edit--command (book)
  "Return the command to commit edits to BOOK to disk."
  `("set_metadata"
    ,@(flatten-list (mapcar (lambda (field)
                              `("-f" ,(format "%s:%s" (calibre-util-uglify-field-name field)
                                             (cl-case field
                                               (authors (string-join (calibre-book-authors book) ","))
                                               (title (calibre-book-title book))
                                               (publisher (calibre-book-publisher book))
                                               (series (calibre-book-series book))
                                               (series-index (calibre-book-series-index book))
                                               (tags (string-join (calibre-book-tags book) ","))))))
                           (calibre-edit--different-fields book (calibre-edit--find-original book))))
    ,(format "%d" (calibre-book-id book))))

(defun calibre-edit-commit-edits (books)
  "Commit edits to BOOKS to disk."
  (calibre-exec--queue-commands (mapcar #'calibre-edit--command books)))

(provide 'calibre-edit)
;;; calibre-edit.el ends here
