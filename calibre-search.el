;;; calibre-search.el --- Filter books based on search criteria.  -*- lexical-binding: t; -*-
;; Copyright (C) 2023  Kjartan Óli Águstsson

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
;; This file provides an interface for filtering the current library
;; on criteria such as Author, Publisher, Tags, etc.

;;; Code:
(require 'transient)
(require 'calibre-db)

(defmacro calibre-search--choice-function (field &optional an plural)
  "Create a function to prompt the user to select a value for FIELD.
FIELD should be the name of a metadata field associated with
books.  A corresponding calibre-db--get-FIELDs function must
exist, i.e. if FIELD is author the function
calibre-db--get-authors must exist.

If AN is non-nil the new function's docstring will read: Prompt
the user to select an FIELD, otherwise it will read: Prompt the
user to select a FIELD.

If PLURAL is non-nil FIELD is assumed to be a plural word, and an s
will not be appended to the calibre-db--get-FIELD function's name."
  `(defun ,(intern (format "calibre-search-chose-%s" field)) ()
     ,(format "Prompt the user to select %s %s" (if an "an" "a") field)
     (interactive)
     (completing-read ,(format "%s:" (capitalize field))
                      (,(intern (format "calibre-db--get-%s%s"
                                        field
                                        (if plural "" "s")))))))

(calibre-search--choice-function "author" t)
(calibre-search--choice-function "publisher")
(calibre-search--choice-function "tag")

(defun calibre-search--operation (args)
  "Return the appropriate symbol for a filter operation.
ARGS is the argument list of a transient command."
  (if (cl-find "--exclude" args :test #'string=) '- '+))

(defmacro calibre-library--search-function (field)
  "Create a function adding a filter for FIELD."
  `(defun ,(intern (format "calibre-library-search-%s" field)) (val &optional args)
     (interactive (list (,(intern (format "calibre-search-chose-%s" field)))
                        (transient-args 'calibre-search)))
     (setf calibre-library--filters (cons
                                     (vector (calibre-search--operation args)
                                             (quote ,(intern field))
                                             val)
                                     calibre-library--filters))
     (calibre-library--refresh)))

(calibre-library--search-function "author")
(calibre-library--search-function "publisher")
(calibre-library--search-function "tag")

(defun calibre-library-clear-last-search ()
  "Clear the last applied search filter."
  (interactive)
  (when calibre-library--filters
    (setf calibre-library--filters (cdr calibre-library--filters)))
  (calibre-library--refresh))

(transient-define-prefix calibre-search ()
  ""
  :transient-suffix 'transient--do-call
  ["Arguments"
   ("-e" "Exclude" "--exclude")]
  ["Search"
   ("a" "Author" calibre-library-search-author)
   ("p" "Publisher" calibre-library-search-publisher)
   ("t" "Tag" calibre-library-search-tag)]
  ["Misc"
   ("u" "Undo" calibre-library-clear-last-search)
   ("c" "Clear" calibre-library-clear-filters)
   ("q" "Exit" transient-quit-one)])

(provide 'calibre-search)
