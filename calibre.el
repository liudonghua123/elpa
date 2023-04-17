;;; calibre.el --- Interact with Calibre libraries from Emacs -*- lexical-binding:t -*-

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
;; Interact with Calibre libraries from within Emacs.
;; View the contents of your library, including much of the metadata
;; associated with each book, and read them, all from within Emacs.

;;; Code:
(require 'calibre-db)

(declare-function calibre-library--refresh "calibre-library.el")

(defgroup calibre nil
  "Interact with a Calibre library."
  :group 'calibre)

(defcustom calibre-calibredb-executable "calibre"
  "The calibredb executable to use."
  :type 'string
  :package-version '("calibre" . "0.1.0"))

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
  :package-version '("calibre" . "0.1.0"))

(defcustom calibre-libraries nil
  "An alist mapping library names to directories."
  :type '(repeat :tag "Libraries" (cons :tag "Library"
                                   (string :tag "Name")
                                   (directory :tag "Location")))
  :package-version '("calibre" . "0.1.0"))

(defvar calibre--books nil)
(defun calibre--books (&optional force)
  "Return the in memory list of books.
If FORCE is non-nil the list is refreshed from the database."
  (when (or force (not calibre--books))
    (setf calibre--books (calibre-db--get-books)))
  calibre--books)

(defun calibre--library-names ()
  "Return a list of the names of defined libraries."
  (mapcar #'car calibre-libraries))

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

(defcustom calibre-format-preferences '(pdf epub)
  "The preference order of file formats."
  :type '(repeat symbol :tag "Format")
  :package-version '("calibre" . "0.1.0"))

(provide 'calibre)
;;; calibre.el ends here
