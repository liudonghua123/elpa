;;; calibre-util.el --- Small utilities that have no better place -*- lexical-binding:t -*-

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

;;; Code:
(require 'parse-time)

(defun calibre-parse-timestamp (timestamp)
  "Return a Lisp timestamp from TIMESTAMP.
TIMESTAMP is a string of the form YYYY-MM-DD HH:MM:SS.xxxxxx+00:00."
  (parse-iso8601-time-string (string-replace " " "T" timestamp)))

(provide 'calibre-util)
;;; calibre-util.el ends here
