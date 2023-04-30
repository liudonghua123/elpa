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

(defun calibre-chose-author ()
  "Prompt the user to select an author."
  (interactive)
  (completing-read "Author: " (calibre-db--get-authors)))

(defun calibre-chose-publisher ()
  "Prompt the user to select a publisher."
  (interactive)
  (completing-read "Author: " (calibre-db--get-publishers)))

(defun calibre-chose-tag ()
  "Prompt the user to select a tag."
  (interactive)
  (completing-read "Tag: " (calibre-db--get-tags)))

(defun calibre-search--operation (args)
  "Return the appropriate symbol for a filter operation.
ARGS is the argument list of a transient command."
  (if (cl-find "--exclude" args :test #'string=) '- '+))

(defun calibre-library-search-author (author &optional args)
  "Add a filter for AUTHOR.
ARGS determines whether the created filter is inclusive or exclusive."
  (interactive (list (calibre-chose-author)
                     (transient-args 'calibre-search)))
  (setf calibre-library--filters (cons `[,(calibre-search--operation args) author ,author]
                                       calibre-library--filters))
  (calibre-library--refresh))

(defun calibre-library-search-publisher (publisher &optional args)
    "Add a filter for PUBLISHER.
ARGS determines whether the created filter is inclusive or exclusive."
  (interactive (list (calibre-chose-publisher)
                     (transient-args 'calibre-search)))
  (setf calibre-library--filters (cons `[,(calibre-search--operation args) publisher ,publisher]
                                       calibre-library--filters))
  (calibre-library--refresh))

(defun calibre-library-search-tag (tag &optional args)
    "Add a filter for TAG.
ARGS determines whether the created filter is inclusive or exclusive."
  (interactive (list (calibre-chose-tag)
                     (transient-args 'calibre-search)))
  (setf calibre-library--filters (cons `[,(calibre-search--operation args) tag ,tag]
                                       calibre-library--filters))
  (calibre-library--refresh))

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
