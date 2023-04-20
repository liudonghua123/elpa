;;; calibre-util.el --- Break circular dependencies in other parts -*- lexical-binding:t -*-

;; Copyright (C) 2023  Kjartan Oli Agustsson

;; Author: Kjartan Oli Agustsson <kjartanoli@disroot.org>
;; Maintainer: Kjartan Oli Agustsson <kjartanoli@disroot.org>

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

;;; Commentary:
;; This file contains functions and variables that might conceptually
;; belong in other files, whose presence in those files would create a
;; circular dependency.

;;; Code:
(require 'calibre)

(defvar calibre--books nil)
(defun calibre--books (&optional force)
  "Return the in memory list of books.
If FORCE is non-nil the list is refreshed from the database."
  (when (or force (not calibre--books))
    (setf calibre--books (calibre-db--get-books)))
  calibre--books)

(defvar calibre--library nil
  "The active library.")

(defun calibre-select-library (&optional library)
  "Prompt the user to select a library from `calibre-libraries'.
If LIBRARY is non-nil, select that instead."
  (interactive)
  (setf calibre--library (if library
                            library
                           (completing-read "Library: " (calibre--library-names) nil t))
        calibre--db nil
        calibre--books nil)
  (calibre-library--refresh t))

(defun calibre--library ()
  "Return the active library.
If no library is active, prompt the user to select one."
  (unless calibre--library
    (calibre-select-library))
  (alist-get calibre--library calibre-libraries nil nil #'string=))

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

(defun calibre-book--print-info (book)
  "Return list suitable as a value of `tabulated-list-entries'.
BOOK is a `calibre-book'."
  (list book
        (with-slots (id title authors publishers series series-index tags formats) book
          (vconcat (mapcar (lambda (x)
                             (let ((column (car x))
                                   (width (cdr x)))
                               (cl-case column
                                 (id (format (format "%%%ds" width) id))
                                 (title title)
                                 (authors (string-join authors ", "))
                                 (publishers (string-join publishers ", "))
                                 (series (if (not series) "" series))
                                 (series-index (if series (format "%.1f" series-index) ""))
                                 (tags (string-join tags ", "))
                                 (formats (string-join (mapcar (lambda (f) (upcase (symbol-name f))) formats) ", ")))))
                           calibre-library-columns)))))

(defun calibre-book--file (book format)
  "Return the path to BOOK in FORMAT."
  (with-slots (path file-name) book
    (message "%S" file-name)
    (file-name-concat (calibre--library)
                      path
                      (message "%s.%s" file-name format))))

(provide 'calibre-util)
;;; calibre-util.el ends here
