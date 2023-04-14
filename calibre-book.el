;;; calibre-book.el --- Create and manage lisp representations of books -*- lexical-binding:t -*-

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

;;; Code:
(require 'eieio)

(require 'calibre-util)

(defclass calibre-book ()
  ((id :initarg :id
       :type number
       :documentation "The ID of the book in the Calibre database.")
   (title :initarg :title
          :type string
          :documentation "The title of the book.")
   (authors :initarg :authors
            :type list
            :documentation "The authors of the book.")
   (publishers :initarg :publishers
               :type list
               :documentation "The publishers of the book.")
   (series :initarg :series
           :initform nil
           :type (or string null)
           :documentation "The series the book is a part of.")
   (series-index :initarg :series-index
                 :initform 1
                 :type real
                 :documentation "The book's position within its series.")
   (formats :initarg :formats
            :type list)
   (timestamp :initarg :timestamp
              :type list)
   (pubdate :initarg :pubdate
            :type list)
   (last-modified :initarg :last-modified
                  :type list)
   (tags :initarg :tags
         :initform '()
         :type list
         :documentation "Tags associated with the book.")
   (path :initarg :path
         :type string
         :documentation "The book's position within the library")
   (file-name :initarg :file-name
              :type string
              :documentation "The book's filename, sans extension.")))

(defmacro calibre-book--slot (slot &optional internal)
  "Create a function to access SLOT of a `calibre-book'.
If INTERNAL is non nil the function name will follow the convention
for private functions."
  `(defun ,(intern (format "calibre-book-%s%s" (if internal "-" "") slot)) (book)
     ,(format "Access the %s slot of a `calibre-book'." slot)
     (slot-value book ',slot)))

(calibre-book--slot id)
(calibre-book--slot title)
(calibre-book--slot authors)
(calibre-book--slot publishers)
(calibre-book--slot series)
(calibre-book--slot series-index)
(calibre-book--slot tags)
(calibre-book--slot formats)
(calibre-book--slot path t)
(calibre-book--slot file-name)

(defun calibre-make-book (entry)
  "Create a `calibre-book' from ENTRY.
ENTRY is a list of the form:
\(ID TITLE SERIES SERIES-INDEX TIMESTAMP PUBDATE LAST-MODIFIED)."
  (seq-let [id title series series-index timestamp pubdate last-modified path] entry
    (calibre-book :id id
                  :title title
                  :authors (calibre-db--get-book-authors id)
                  :publishers (calibre-db--get-book-publishers id)
                  :series series
                  :series-index series-index
                  :timestamp (calibre-parse-timestamp timestamp)
                  :pubdate (calibre-parse-timestamp pubdate)
                  :last-modified (calibre-parse-timestamp last-modified)
                  :tags (calibre-db--get-book-tags id)
                  :formats (calibre-db--get-book-formats id)
                  :path path
                  :file-name (calibre-db--get-book-file-name id))))

(defun calibre-book--print-info (book)
  "Return list suitable as a value of `tabulated-list-entries'.
BOOK is a `calibre-book'."
  (list book
        (with-slots (id title authors series series-index tags formats) book
          `[,(int-to-string id)
            ,title
            ,(string-join authors ", ")
            ,(if (not series) "" series)
            ,(if series (format "%.1f" series-index) "")
            ,(string-join tags ", ")
            ,(string-join (mapcar (lambda (f) (upcase (symbol-name f))) formats) ", ")])))

(defun calibre-book--file (book format)
  "Return the path to BOOK in FORMAT."
  (with-slots (path file-name) book
    (message "%S" file-name)
    (file-name-concat calibre-library-dir
                      path
                      (message "%s.%s" file-name format))))

(defun calibre-book--pick-format (book)
  "Return the preferred format for BOOK."
  (car (seq-intersection calibre-format-preferences
                         (calibre-book-formats book))))

(defun calibre-book-sort-by-series (a b)
  "Return t if A should appear before B when sorting by series."
  (if (not (calibre-book-series a))
      t
    (if (not (calibre-book-series b))
        nil
      (cl-case (compare-strings (calibre-book-series a) nil nil
                                  (calibre-book-series b) nil nil)
          (-1 t)
          ((eq t) (> (calibre-book-series-index a)
                     (calibre-book-series-index b)))
          (1 nil)))))

(provide 'calibre-book)
;;; calibre-book.el ends here
