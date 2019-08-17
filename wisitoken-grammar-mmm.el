;;; Define multi-major-mode stuff for wisitoken-grammar mode.  -*- lexical-binding:t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'mmm-mode)

(mmm-add-classes
 '((wisi-action
    :match-submode wisitoken-grammar-mmm-submode
    :face mmm-code-submode-face
    :front "%("
    :back ")%"
    :insert ((?a wisi-action nil @ "%(" @ "" _ "" @ ")%")))
   (wisi-code
    :match-submode wisi-mmm-submode
    :face mmm-code-submode-face
    :front "%{"
    :back "}%"
    :insert ((?a wisi-code nil @ "%{" @ "" _ "" @ "}%")))
   ))

(defvar wisitoken-grammar-action-mode) ;; in wisitoken-grammar-mode.el
(defun wisitoken-grammar-mmm-submode (_delim)
  "for :match-submode"
  wisitoken-grammar-action-mode)

(add-to-list 'mmm-mode-ext-classes-alist '(wisitoken-grammar-mode nil wisi-action))
(add-to-list 'mmm-mode-ext-classes-alist '(wisitoken-grammar-mode nil wisi-code))

(provide 'wisitoken-grammar-mmm)
;;; end of file
