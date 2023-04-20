;;; calibre-book.el --- Create and manage Lisp representations of books -*- lexical-binding:t -*-

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
;; Functions for creating and accessing the values of Lisp
;; representations of books

;;; Code:
(require 'eieio)
(require 'parse-time)

(defun calibre-parse-timestamp (timestamp)
  "Return a Lisp timestamp from TIMESTAMP.
TIMESTAMP is a string of the form YYYY-MM-DD HH:MM:SS.xxxxxx+00:00."
  (parse-iso8601-time-string (string-replace " " "T" timestamp)))

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

(defcustom calibre-format-preferences '(pdf epub)
  "The preference order of file formats."
  :type '(repeat symbol :tag "Format")
  :package-version '("calibre" . "0.1.0")
  :group 'calibre)

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
