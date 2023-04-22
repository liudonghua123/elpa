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
(require 'calibre)
(require 'calibre-book)

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

(defvar calibre--db nil)
(defun calibre--db ()
  "Return the metadata database."
  (unless calibre--db
    (let ((file-name (file-name-concat (calibre--library) "metadata.db")))
      (if (not (file-exists-p file-name))
          (progn
            (message "Metedata database %s does not exist.  Add some books to the library to create it." file-name)
            (setf calibre--db nil))
        (setf calibre--db
              (sqlite-open
               file-name)))))
  calibre--db)

(defun calibre-db--get-books ()
  "Return all books in the Calibre library `calibre-library-dir'."
  (if (not (calibre--db))
      nil
    (mapcar #'calibre-make-book
            (sqlite-select (calibre--db)
                           "SELECT books.id, title, series.name, series_index, timestamp, pubdate, last_modified, path
FROM books
LEFT JOIN books_series_link sl ON books.id = sl.book
LEFT JOIN series ON sl.series = series.id;"))))

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
    (when buffer
      (with-current-buffer buffer
        (setf tabulated-list-entries
              (mapcar #'calibre-book--print-info
                      (calibre--books force)))
        (tabulated-list-print)))))

(defcustom calibre-library-columns '((id . 4)
                                     (title . 35)
                                     (authors . 20)
                                     (publishers . 10)
                                     (series . 15)
                                     (series-index . 3)
                                     (tags . 10)
                                     (formats . 10))
  "The metadata fields to display in the library buffer.
Each entry is a key identifying a metadata field and the width that
column should have."
  :type '(repeat (cons
                  :tag "Column"
                  (choice
                   :tag "Attribute"
                   (const :tag "ID" id)
                   (const :tag "Title" title)
                   (const :tag "Author(s)" authors)
                   (const :tag "Publisher(s)" publishers)
                   (const :tag "Series" series)
                   (const :tag "Series Index" series-index)
                   (const :tag "Tags" tags)
                   (const :tag "Formats" formats))
                  (integer :tag "Width")))
  :set (lambda (symbol value)
         (set-default symbol value)
         (calibre-library--set-header)
         (calibre-library--refresh))
  :package-version '("calibre" . "0.1.0")
  :group 'calibre)

(defun calibre-library--set-header ()
  "Set the header of the Library buffer."
  (let ((buffer (get-buffer calibre-library-buffer)))
    (when buffer
      (with-current-buffer buffer
        (setf tabulated-list-format (calibre-library--header-format))))))

(defun calibre-library--header-format ()
  "Create the header for the Library buffer.
Return a vector suitable as the value of `tabulated-list-format'
with values determined by `calibre-library-columns'."
  (vconcat
   (mapcar (lambda (x)
             (let ((column (car x))
                   (width (cdr x)))
               (cl-case column
                 (id `("ID" ,width (lambda (a b)
                                     (< (calibre-book-id (car a))
                                        (calibre-book-id (car b))))
                       :right-align t))
                 (title `("Title" ,width t))
                 (authors `("Author(s)" ,width t))
                 (publishers `("Publisher(s)" ,width t))
                 (series `("Series" ,width (lambda (a b)
                                             (calibre-book-sort-by-series (car a) (car b)))))
                 (series-index `("#" ,width (lambda (a b)
                                              (calibre-book-sort-by-series (car a) (car b)))
                                 :right-align t))
                 (tags `("Tags" ,width))
                 (formats `("Formats" ,width)))))
           calibre-library-columns)))

(defun calibre-book--print-info (book)
  "Return list suitable as a value of `tabulated-list-entries'.
BOOK is a `calibre-book'."
  (list book
        (with-slots (id title authors publishers series series-index tags formats) book
          (vconcat (mapcar (lambda (x)
                             (let ((column (car x)))
                               (cl-case column
                                 (id (int-to-string id))
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

(provide 'calibre-db)
;;; calibre-db.el ends here
