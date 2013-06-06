;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : float-changes.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Mar 20 16:56:37 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Aug 17 23:36:58 1992
;;;; Update Count    : 12
;;;; 
;;;; PURPOSE
;;;; 	Changes to float.el to support integer and floats in the same 
;;;; calculations (with corercion to floats).
;;;; TABLE OF CONTENTS
;;;; 	i.	New Variables and constants
;;;;	I.	Saved changes from old float.el
;;;;
;;;; 
;;;; Copyright 1991, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'float-changes)

;; (if (fboundp 'proclaim-inline)
;;   (proclaim-inline
;;     float-stringp
;; ))


;;;
;;; 	i.	New Variables and constants
;;;

(defconst floating-point-leading-digits-regexp
   "^\\([0-9]*\\)\\(\\.[0-9]*\\|\\)$")

(defconst integer-regexp "^[ \t]*\\(-?\\)\\([0-9]+\\)[ \t]*$"
  "Regular expression to match integer numbers.  Exact matches:
1 - minus sign
2 - integer part
3 - end of string")

(defconst floating-point-list-regexp
  "^[ \t]*\\((quote\\)[ ]*\
\\(([0-9]+\\)[ ]+\\(\\.\\)[ ]+\
\\(-?\\)\
\\([0-9]+\\)\
))"
  "Regular expression to match old (Rosenblatt) floating point numbers as 
lists in strings.
Extract matches:
0 - leading spaces
1 - quote
2 - more spaces
3 - ( and leading number
4 - dot, spaces
5 - optional negative sign
5 - more numbers
6 - two more closing parens
")

(defconst floating-point-regexp
  "^[ \t]*\\(-?\\)\\([0-9]*\\)\
\\(\\.\\([0-9]*\\)\\|\\)\
\\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
  "Regular expression to match floating point numbers.  Extract matches:
1 - minus sign
2 - integer part
4 - fractional part
8 - minus sign for power of ten
9 - power of ten
")


;; taken from float.el
(defsubst extract-match (str i)		; used after string-match
  (condition-case ()
      (substring str (match-beginning i) (match-end i))
    (error "")))

;(float-stringp "23.0000")
;(float-stringp ".0000")
;(float-stringp "   ")
;(float-stringp "-")
;(float-stringp "   .")
;(float-stringp "(quote (13 . -123123))")

;; (defun float-stringp (astring)
;;   (and (or (string-match floating-point-regexp astring 0)
;;            (string-match floating-point-list-regexp astring 0))
;;        (not (string-match "^[-. \t]*$" astring 0))))

;; (number-stringp "123. 2 ")
;; (setq astring " e6")
;; number-stringp 
(defsubst dismal-number-stringp (astring)
  (and (or (string-match floating-point-regexp astring 0)
           (string-match floating-point-list-regexp astring 0)
           (string-match integer-regexp astring 0))
       (not (string-match "^[-. \t]*$" astring 0))))

(defsubst integer-stringp (astring) ;(integer-stringp "23")
  (string-match integer-regexp astring 0))

;; the - may need a \\, but doesn't look like it.  3-Aug-92 -FER
; (defconst floating-point-regexp
;   "^[ \t]*\\(-?\\)\\([0-9]*\\)\
; \\(\\.\\([0-9]*\\)\\|\\)\
; \\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
;   "Regular expression to match floating point numbers.  Extract matches:
; 1 - minus sign
; 2 - integer part
; 4 - fractional part
; 8 - minus sign for power of ten
; 9 - power of ten
; ")


;;;
;;;	I.	Saved changes from old float.el
;;;

;; not used anywhere, commented out, 2-Jan-97 -FER
;; 
;; (defun xor (a b)			; logical exclusive or
;;   (and (or a b) (not (and a b))))


;; (float-to-string 3e4)

(defsubst float-to-string (fnum &optional sci)
  "Convert the floating point number to a decimal string.
Optional second argument non-nil means use scientific notation."
  (if sci 
     (format "%e" fnum)
     (format "%s" fnum)))
