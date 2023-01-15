;;; typo.el --- Completion style using typo analysis -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022, 2023  Philip Kaludercic

;; Author: Philip Kaludercic <philip.kaludercic@fau.de>
;; URL: https://wwwcip.cs.fau.de/~oj14ozun/src+etc/typo.el
;; Version: $Id: typo.el,v 1.2 2023/01/15 12:42:07 oj14ozun Exp oj14ozun $
;; Package-Version: 1
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Typo.el implements a Norvig-Style[0] spell-corrector for Emacs'
;; completion system.
;;
;; To initialize this completion style, evaluate
;;
;;    (add-to-list 'completion-styles 'typo t)
;;
;; or configure the corresponding code in your initialisation file.
;;
;; [0] https://norvig.com/spell-correct.html

;;; Code:

(eval-when-compile (require 'inline))

(defgroup typo nil
  "Completion style using typo analysis."
  :group 'minibuffer)

(defcustom typo-level 8
  "Number of edits from the current word to a completion."
  :type 'natnum)

(defcustom typo-shrink 1
  "Number of characters a word may shrink."
  :type 'natnum)

(defcustom typo-expand 4
  "Number of characters a word may expand."
  :type 'natnum)

(define-inline typo--test (word key)
  (inline-letevals (word key)
    (inline-quote
     (and (<= (string-distance ,word ,key) typo-level)
	  (<= (- typo-shrink) (- (length ,word) (length ,key)) typo-expand)))))

(defun typo-edits (word collection pred)
  "Generate a list of all multi-edit typos of WORD.
Only words that are in the COLLECTION and satisfy PRED will be
returned.  The variable `typo-level' specifies how many
single-letter typos are searched."
  (let (new-words)
    (cond
     ((and (listp collection) (consp (car collection))) ;alist
      (dolist (entry collection new-words)
	(let ((key (car entry)))
	  (when (symbolp key)
	    (setq key (symbol-name key)))
	  (when (typo--test word key)
	    (push key new-words)))))
     ((listp collection)		;regular list
      (dolist (entry collection new-words)
	(when (typo--test word entry)
	  (push entry new-words))))
     ((hash-table-p collection)
      (maphash
       (lambda (key _freq)
	 (when (typo--test word key)
	   (push key new-words)))
       collection)
      new-words)
     ((obarrayp collection)
      (mapatoms
       (lambda (atom)
	 (setq atom (symbol-name atom))
	 (when (typo--test word atom)
	   (push atom new-words)))
       collection)
      new-words)
     ((functionp collection)
      (typo-edits word (funcall collection "" pred t) pred)))))

;;;###autoload
(defun typo-all-completions (string collection pred _point)
  "Generate all  versions of the STRING using COLLECTION.
COLLECTION are as defined in `all-completions'."
  (typo-edits string collection pred))

;;;###autoload
(defun typo-try-completion (string collection pred _point &optional _metadata)
  "Generate the most probable version of STRING using COLLECTION.
COLLECTION are as defined in `try-completion'."
  (let* ((result (typo-edits string collection pred))
	 (best (car result)))
    (dolist (other (cdr result))
      (when (< (string-distance string other)
	       (string-distance string best))
	(setq best other)))
    (and best (cons best (length best)))))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(typo typo-try-completion typo-all-completions
	       "Typo-Fixing completion
I.e. when completing \"foobor\", with \"foobar\" in the
completion table, this style would attempt replace it with
\"foobar\", because the two strings are close by."))

(provide 'typo)

;;; typo.el ends here
