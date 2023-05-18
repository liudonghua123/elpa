;;; calibre-cli.el --- Fallback CLI interface when SQLite is not available  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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
;; This file contains an interface to calibre using calibredb and JSON
;; parsing instead of SQLite.  This interface is significantly slower
;; than the SQLite interface, and should therefore only be used when
;; SQLite support is not available.

;;; Code:
(require 'calibre)
(require 'calibre-book)
(require 'calibre-util)

(defun calibre-cli--get-books ()
  "Return all books in the Calibre library `calibre-library-dir'."
  (let ((json (with-temp-buffer
                (call-process calibre-calibredb-executable
                              nil
                              t
                              nil
                              "list"
                              "--with-library"
                              (calibre--library)
                              "-f"
                              "all"
                              "--for-machine")
                (goto-char (point-min))
                (json-parse-buffer :object-type 'alist :array-type 'list))))
    (mapcar #'calibre-cli--make-book json)))

(defun calibre-cli--make-book (json)
  "Make a `calibre-book' from JSON."
  (let-alist json
    (make-calibre-book :id .id
                       :title .title
                       :authors (calibre-cli--parse-authors .authors)
                       :publisher .publisher
                       :series .series
                       :series-index .series_index
                       :timestamp (calibre-cli--parse-timestamp .timestamp)
                       :pubdate (calibre-cli--parse-timestamp .pubdate)
                       :last-modified (calibre-cli--parse-timestamp .last_modified)
                       :tags .tags
                       :formats (calibre-cli--parse-formats .formats)
                       :path (calibre-cli--parse-path .formats)
                       :file-name (calibre-cli--parse-file-name .formats))))

(defun calibre-cli--parse-authors (authors)
  "Parse AUTHORS a string, into a list of authors.
AUTHORS should be a comma separated string."
  (string-split authors ","))

(defun calibre-cli--parse-timestamp (timestamp)
  "Parse TIMESTAMP into a Lisp timestamp."
  (iso8601-parse timestamp))

(defun calibre-cli--parse-formats (files)
  "Return the formats of the book whose files are FILES."
  (mapcar (lambda (f)
            (intern (file-name-extension f)))
          files))

(defun calibre-cli--parse-path (files)
  "Return the path within the library of the book whose files are FILES."
  (string-replace (calibre--library)
                  ""
                  (file-name-directory (car files))))

(defun calibre-cli--parse-file-name (files)
  "Return the File Name of the book whose files are FILES."
  (file-name-base (car files)))

(defmacro calibre-cli--search-operation (field)
  "Create a function to search for books matching FIELD."
  `(defun ,(intern (format "calibre-cli--get-%s-books" field)) (,field)
     (with-temp-buffer
       (call-process calibre-calibredb-executable
                     nil
                     t
                     nil
                     "search"
                     "--with-library"
                     (calibre--library)
                     (format ,(format "%s:=%%s" field) ,field))
       (mapcar #'cl-parse-integer
               (string-split
                (buffer-substring-no-properties (point-min) (point-max))
                ",")))))

(calibre-cli--search-operation series)
(calibre-cli--search-operation publisher)
(calibre-cli--search-operation format)
(calibre-cli--search-operation tag)
(calibre-cli--search-operation author)

(provide 'calibre-cli)
;;; calibre-cli.el ends here
