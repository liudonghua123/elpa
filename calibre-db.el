;;; calibre-db.el --- Interact with the Calibre database -*- lexical-binding:t -*-

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
;; Fetch data from the library database.

;;; Code:
(defvar calibre--db nil)
(defun calibre--db ()
  "Return the metadata database."
  (unless calibre--db
    (setf calibre--db
          (sqlite-open
           (file-name-concat calibre-library-dir "metadata.db"))))
  calibre--db)

(defun calibre-db--get-books ()
  "Return all books in the Calibre library `calibre-library-dir'."
  (mapcar #'calibre-make-book
          (sqlite-select (calibre--db)
                         "SELECT books.id, title, series.name, series_index, timestamp, pubdate, last_modified, path
FROM books
LEFT JOIN books_series_link sl ON books.id = sl.book
LEFT JOIN series ON sl.series = series.id;")))

(defun calibre-db--get-book-authors (id)
  "Return a list of authors for the book identified by ID."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT authors.name
FROM authors
INNER JOIN books_authors_link al ON authors.id = al.author
WHERE al.book = ?" `[,id])))

(defun calibre-db--get-book-publishers (id)
  "Return a list of publishers for the book identified by ID."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT publishers.name
FROM publishers
INNER JOIN books_publishers_link pl ON publishers.id = pl.publisher
WHERE pl.book = ?" `[,id])))

(defun calibre-db--get-book-file-name (id)
  "Return the file name, sans extension, of the book identified by ID."
  (car (car (sqlite-select (calibre--db)
                           "SELECT name
FROM data
WHERE book = ?" `[,id]))))

(defun calibre-db--get-book-tags (id)
  "Return a list of tags for the book identified by ID."
  (flatten-list (sqlite-select (calibre--db)
                               "SELECT tags.name
FROM books
INNER JOIN books_tags_link tl ON books.id = tl.book
INNER JOIN tags ON tl.tag = tags.id
WHERE books.id = ?"
                               `[,id])))

(defun calibre-db--get-book-formats (id)
  "Return a list of formats for the book identified by ID."
  (mapcar (lambda (f)
            (intern (downcase f)))
          (flatten-list (sqlite-select (calibre--db)
                               "SELECT format FROM data WHERE book = ?"
                               `[,id]))))

(provide 'calibre-db)
;;; calibre-db.el ends here
