;;; vcard.el --- vcard parsing and formatting routines

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Created: 1997-09-27

;; $Id: vcard.el,v 1.1 1997/10/01 11:55:52 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(defun vcard-parse-string (raw)
  (save-match-data
    (let ((raw-pos 0)
          (vcard-data nil)
          key data)
      (string-match "^begin:[ \t]*vcard[ \t]*[\r\n]+" raw raw-pos)
      (setq raw-pos (match-end 0))
      (while (and (< raw-pos (length raw))
                  (string-match "^\\([^:]+\\):[ \t]+\\(.*\\)[ \t]*[\n\r]+"
                                raw raw-pos))
        (setq key (vcard-matching-substring 1 raw))
        (setq data (vcard-matching-substring 2 raw))
        (setq raw-pos (match-end 0))
        (cond
         ((string= key "end")
          (setq raw-pos (length raw)))
         (t
          (setq vcard-data
                (vcard-set-alist-slot vcard-data
                                      (vcard-split-string key ";")
                                      (vcard-split-string data ";"))))))
      (nreverse vcard-data))))

(defun vcard-ref (key vcard-data)
  (cond ((listp key)
         (vcard-nested-alist-assoc key vcard-data))
        ((and (stringp key)
              (save-match-data
                (string-match ";" key)))
         (vcard-nested-alist-assoc (vcard-split-string key ";") vcard-data))
        ((stringp key)
         (cdr (assoc key vcard-data)))))


(defun vcard-nested-alist-assoc (keys alist)
  (while (and keys alist)
    (setq alist (cdr (assoc (car keys) alist)))
    (setq keys (cdr keys)))
  alist)

(defun vcard-set-alist-slot (alist key-list value)
  (let* ((key (car key-list))
         (elt (assoc key alist)))
    (setq key-list (cdr key-list))
    (cond ((and (cdr elt) key-list)
           (vcard-set-alist-slot (cdr elt) key-list value))
          ((and elt key-list)
           (setcdr elt (vcard-set-alist-slot nil key-list value)))
          (elt (setcdr elt value))
          (t
           (let ((new))
             (setq key-list (nreverse (cons key key-list)))
             (while key-list
               (if new
                   (setq new (cons (car key-list) (cons new nil)))
                 (setq new (cons (car key-list) value)))
               (setq key-list (cdr key-list)))

             (cond ((null alist)
                    (setq alist (cons new nil)))
                   (t
                    (setcdr alist (cons (car alist) (cdr alist)))
                    (setcar alist new))))))
    alist))


;; Return substring matched by last search.
;; N specifies which match data pair to use
;; Value is nil if there is no Nth match.
;; If STRING is not specified, the current buffer is used.
(defun vcard-matching-substring (n &optional string)
  (if (match-beginning n)
      (if string
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

;; Split STRING at occurences of SEPARATOR.  Return a list of substrings.
;; SEPARATOR can be any regexp, but anything matching the separator will
;; never appear in any of the returned substrings.
(defun vcard-split-string (string separator)
  (let* ((list nil)
         (pos 0))
    (save-match-data
      (while (string-match separator string pos)
        (setq list (cons (substring string pos (match-beginning 0)) list))
        (setq pos (match-end 0)))
      (nreverse (cons (substring string pos) list)))))

(provide 'vcard)

;;; vcard.el ends here.
