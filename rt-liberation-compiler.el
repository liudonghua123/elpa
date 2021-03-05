;;; rt-liberation-compiler.el --- Emacs interface to RT  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Yoni Rabkin <yrk@gnu.org>
;; Authors: Aaron S. Hawley <aaron.s.hawley@gmail.com>, John Sullivan <johnsu01@wjsullivan.net>
;; Maintainer: Yoni Rabkin <yrk@gnu.org>
;; Keywords: rt, tickets
;; url: http://www.nongnu.org/rtliber/

;; This file is a part of rt-liberation.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Installation and Use:
;;
;; Detailed instructions for installation and use can be found in the
;; rt-liberation manual, in the doc/ directory of the distribution.

;;; History:
;;
;; Started near the end of 2008.


;;; Code:
(require 'cl-lib)


;;; ------------------------------------------------------------------
;;; variables and constants
;;; ------------------------------------------------------------------
(defvar rt-liber-content-string "Content LIKE"
  "String representation of \"content\" query tag.")

(defvar rt-liber-subject-string "Subject LIKE"
  "String representation of \"subject\" query tag.")

(defvar rt-liber-email-address-string "Requestor.EmailAddress LIKE"
  "String representation of \"Requestor.EmailAddress\" query tag.")

(defvar rt-liber-content-not-string "Content NOT LIKE"
  "String representation of \"content\" query tag.")

(defvar rt-liber-subject-not-string "Subject NOT LIKE"
  "String representation of \"subject\" query tag.")

(defvar rt-liber-resolved-string "Resolved"
  "String representation of \"resolved\" query tag.")

(defvar rt-liber-lastupdated-string "LastUpdated"
  "String representation of \"lastupdated\" query tag.")

(defvar rt-liber-email-address-not-string "Requestor.EmailAddress NOT LIKE"
  "String representation of \"Requestor.EmailAddress\" query tag.")

(defvar rt-liber-created-string "Created"
  "String representation of \"created\" query tag.")


;;; --------------------------------------------------------
;;; TicketSQL compiler
;;; --------------------------------------------------------
(eval-and-compile ;; for use in macro `rt-liber-compile-query'
  (defun rt-liber-bool-p (sym)
    "Return t if SYM is a boolean operator, otherwise nil."
    (member sym '(and or)))

  (defun rt-liber-attrib-p (sym)
    "Return t if SYM is a ticket attribute, otherwise nil."
    (member sym '(id owner status subject content queue lastupdatedby
		     email-address)))

  (defun rt-liber-time-p (sym)
    "Return t if SYM is a temporal attribute, otherwise nil."
    (member sym '(created lastupdated resolved)))

  (defun rt-liber-negation-p (sym)
    (member sym '(not)))

  (defun rt-liber-reduce (op seq)
    "Reduce-OP with SEQ to a string of \"s0 op s1 op s2..\"."
    (if seq
	(cl-reduce
	 #'(lambda (a b)
	     (format "%s %s %s" a op b))
	 seq)
      ""))

  (defun rt-liber-make-interval (pred before after)
    "Return a formatted TicketSQL interval.
PRED   temporal attribute predicate.
BEFORE date before predicate.
AFTER  date after predicate."
    (when (string= before "") (setq before nil))
    (when (string= after "") (setq after nil))
    (concat
     (if before (format "%s < '%s'" pred before) "")
     (if (and before after) (format " AND ") "")
     (if after (format "%s > '%s'" pred after) ""))))

(defmacro rt-liber-compile-query (query &optional n)
  "Compile sexp-based QUERY into TicketSQL."
  (cond ((null query) `"")
	((stringp query) `,query)
	((rt-liber-bool-p query) `,(upcase (format "%s" query)))
	;; attribute (positive)
	((and (rt-liber-attrib-p query)
	      (not n))
	 `,(cond ((equal query 'content) rt-liber-content-string)
		 ((equal query 'subject) rt-liber-subject-string)
		 ((equal query 'email-address) rt-liber-email-address-string)
		 (t (capitalize (format "%s =" query)))))
	;; attribute (negation)
	((and (rt-liber-attrib-p query)
	      n)
	 `,(cond ((equal query 'content) rt-liber-content-not-string)
		 ((equal query 'subject) rt-liber-subject-not-string)
		 ((equal query 'email-address) rt-liber-email-address-not-string)
		 (t (capitalize (format "%s !=" query)))))
	;; time
	((rt-liber-time-p query)
	 `,(cond ((equal query 'created) rt-liber-created-string)
		 ((equal query 'lastupdated) rt-liber-lastupdated-string)
		 ((equal query 'resolved) rt-liber-resolved-string)))
	((and (listp query)
	      (rt-liber-time-p (car query)))
	 `(rt-liber-make-interval
	   (rt-liber-compile-query ,(car query))
	   (rt-liber-compile-query ,(cadr query))
	   (rt-liber-compile-query ,(caddr query))))
	;; function (known at compile time?)
	((and query
	      (listp query)
	      (not (rt-liber-bool-p (car query)))
	      (not (rt-liber-negation-p (car query)))
	      (functionp (car query)))
	 `(format "%s" ,query))
	;; negation attribute pairs
	((and (listp query)
	      (rt-liber-negation-p (car query))
	      (rt-liber-attrib-p (caadr query)))
	 `(format "%s '%s'"
		  (rt-liber-compile-query ,(caadr query) t) ; negate
		  (rt-liber-compile-query ,(cadadr query))))
	;; attribute pairs
	((and (listp query)
	      (rt-liber-attrib-p (car query)))
	 `(format "%s '%s'"
		  (rt-liber-compile-query ,(car query))
		  (rt-liber-compile-query ,(cadr query))))
	;; splice boolean operators
	((and (listp query)
	      (rt-liber-bool-p (car query)))
	 `(rt-liber-reduce (rt-liber-compile-query ,(car query))
			   (rt-liber-compile-query ,(cdr query))))
	;; compound statements
	((and (listp query)
	      (not (cdr query)))
	 `(list (rt-liber-compile-query ,(car query))))
	((listp query)
	 `(append
	   (list (rt-liber-compile-query ,(car query)))
	   (rt-liber-compile-query ,(cdr query))))
	;; free variable
	((and query
	      (symbolp query))
	 `(format "%s" ,query))
	(t (error "cannot compile query %s" query))))


(provide 'rt-liberation-compiler)

;;; rt-liberation-compiler.el ends here.
