;;; el-search-pp.el --- Further prettifications for pp with means of el-search -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2018_01_14
;; Keywords: lisp


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This files provides a minor mode `el-search-pretty-pp-mode' that
;; enhances pp.el to produce even prettier results.  Since
;; el-search-query-replace uses pp to format replacement, this has
;; also an effect on the insertions done by this command.
;;
;;
;; Bugs, Known Limitations:
;;
;; This doesn't work with `cl-print'ed contents



;;; Code:

(require 'el-search)
(require 'el-search-x)

(defun el-search-prettify-let-likes ()
  ;; Remove possible line break directly after the macro name
  (let ((let-like-matcher (el-search-make-matcher el-search--match-let-like-pattern)))
    (save-excursion
      (while (el-search--search-pattern-1 let-like-matcher t)
        (when (looking-at "(\\(\\_<\\(\\w\\|\\s_\\)+\\_>\\*?\\) *\n")
          (save-excursion
            (goto-char (match-end 1))
            (delete-region
             (point)
             (progn (skip-chars-forward " \t\n") (point)))
            (insert " "))
          (indent-sexp))
        (el-search--skip-expression nil 'read)))))

(defun el-search-prettify-let-like-bindings ()
  (let ((let-like-binding-matcher (el-search-make-matcher '(and (let-like-binding) `(,_ ,_)))))
    (save-excursion
      (while (el-search--search-pattern-1 let-like-binding-matcher t)
        (let ((deleted-line-break nil))
          (save-excursion
            (when (setq deleted-line-break
                        (progn (down-list 1)
                               (goto-char (scan-sexps (point) 1))
                               (looking-at "[\s\t]*\n[\s\t]+")))
              (delete-region (match-beginning 0) (match-end 0))
              (insert " ")))
          (when deleted-line-break (indent-sexp))
          (el-search--skip-expression nil 'read))))))

(defun el-search-prettify-huge-lists ()
  (save-excursion
    (while (el-search--search-pattern-1 (el-search-make-matcher '(pred listp)) t nil)
      (pcase-let ((`(,this-list ,bound) (save-excursion (list (el-search-read (current-buffer))
                                                              (copy-marker (point))))))
        (when (and (not (macrop (car this-list))) ; FIXME: find a solution for funs and macros
                   (or
                    (< 60 (- bound (point)))
                    (and
                     (null (cdr (last this-list))) ;FIXME: what about dotted or circular lists?
                     (nthcdr 10 this-list)
                     (not (cl-every (lambda (elt) (and (atom elt) (not (stringp elt))))
                                    this-list)))))
          (save-excursion
            (down-list 1)
            (while (el-search-forward '_ bound t)
              (goto-char (scan-sexps (point) 1))
              (unless (or (looking-at "$") (not (save-excursion (el-search-forward '_ bound t))))
                (insert "\n"))))
          (indent-sexp)))
      (el-search--skip-expression nil 'read)))
  (indent-sexp))

(defun el-search-prettify-tiny-lists ()
  (save-excursion
    (while (el-search--search-pattern-1 (el-search-make-matcher '(pred listp)) t nil)
      (pcase-let ((bound (copy-marker (scan-sexps (point) 1))))
        (when (and (< (count-matches "[^[:space:]]" (point) bound) 45)
                   (save-excursion (search-forward-regexp "\n" bound t)))
          (save-excursion
            (while (search-forward-regexp "\n[[:space:]]*" bound t)
              (replace-match " ")))
          (indent-sexp)))
      (el-search--skip-expression nil 'read)))
  (indent-sexp))


(defvar el-search-prettify-functions
  '(el-search-prettify-let-likes
    el-search-prettify-let-like-bindings
    el-search-prettify-huge-lists
    el-search-prettify-tiny-lists))

(defgroup el-search-pp '() "Doc..." :group 'el-search)

(defcustom el-search-pretty-pp nil
  "Doc..."
  :type 'boolean)

(defun el-search-pp-buffer ()
  (emacs-lisp-mode)
  (goto-char (point-min))
  (mapc (lambda (fun) (save-excursion (funcall fun)))
        el-search-prettify-functions))


(provide 'el-search-pp)

;;; el-search-pp.el ends here

