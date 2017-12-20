;;; sscell.el --- An implementation of abstract spreadsheet cell objects    -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2017_12_11
;; Keywords: lisp
;; Version: 0.1
;; Package-Requires: ((emacs "25"))


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

;; This package implements objects that are an abstract version of
;; spreadsheet cells.  Note that this has nothing to do with a
;; spreadsheet application (though you could use sscells to implement
;; one, but this is not the goal of this package) - these sscells are
;; a data type useful for general Elisp programming.
;;
;; An sscell is an object containing a value field, a calculation rule
;; to update that cell's value, and a set of dependencies.  There are
;; two types of dependencies: implicit and static dependecies.  Static
;; dependencies are specified when creating an sscell:
;;
;;   (sscell-make static-deps rule)
;;
;; where RULE is the calculation rule for the returned cell.  You ask
;; an sscell for its value with (sscell-get S).  The value is
;; calculated when it has not been calculated yet, or when one of the
;; dependencies changed.
;;
;; Static dependencies are expressions that are evaluated to check
;; whether the saved value is still valid.  Whenever one of these
;; expressions evaluates to a value different from the last time, the
;; sscell counts as invalid, and any call to `sscell-get' will trigger
;; a recomputation of the cell value.  "Different value" means not
;; `eq' by default, but you can also specify a different test
;; predicate.
;;
;; Whenever the calculation of the value of an sscell refers to a
;; value of another sscell, the value of that other sscell is
;; remembered as an implicit dependency.  Whenever the cell value of
;; that second cell changes, the first cell counts as invalid.
;;
;; You can change the computation expression of an sscell with
;; `sscell-set', and also set the value directly with
;; `sscell-set-value' (which nullifies all dependencies and gives you
;; something like an "input cell").
;;
;; This package also implements let-like binding constructs
;; `sscell-let' and `sscell-let*'.  These constructs create lazy
;; bindings using sscells implicitly.  The created bindings are
;; silently recomputed when referenced and any declared dependencies
;; changed.
;;
;; Examples: ...


;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))


(defvar sscell--tag (make-symbol "sscell"))
(defvar sscell--new-dynamic-deps nil)
(defvar sscell--asking-sscell nil)

(defun sscellp (object)
  "Return non-nil when the OBJECT is an sscell."
  (eq (car-safe object) sscell--tag))

(defmacro sscell-make--1 (static-deps rule &optional reuse-cons)
  ;; Like `sscell-make', but with an additional optional arg REUSE-CONS:
  ;; When specified, it must be a cons cell C with car sscell--tag, and
  ;; the return value is the manipulated cons C.
  (declare (indent 1))
  (cl-callf or static-deps '(t))
  (let ((last-result      (make-symbol "last-result"))
        (last-static-dep-results (make-symbol "last-dep-results"))
        (new-dep-results  (make-symbol "new-dep-results"))
        (instruction      (make-symbol "instruction"))
        (static-tests (cl-maplist
                       (lambda (more-deps) (let ((dep (car more-deps)))
                                        (if (not (eq (car-safe dep) :test))
                                            '#'eq
                                          (prog1 (nth 1 dep)
                                            (setcar more-deps (nth 2 dep))))))
                       static-deps))
        (dynamic-deps              (make-symbol "dynamic-deps"))
        (last-dynamic-dep-results  (make-symbol "last-dynamic-dep-results"))
        (self                      (make-symbol "self"))
        (cell-invalid-p            (make-symbol "cell-invalid-p"))
        (get-value                 (make-symbol "get-value"))
        (arg                       (make-symbol "arg")))
    `(let ((,get-value (lambda () ,rule))
           ,last-result
           (,last-static-dep-results nil)
           (,dynamic-deps nil)
           (,last-dynamic-dep-results nil))
       (let ((,cell-invalid-p
              (lambda ()
                (let ((,new-dep-results (list ,@static-deps)))
                  (unless (and ,last-static-dep-results
                               (cl-every #'identity
                                         (cl-mapcar #'funcall (list ,@static-tests)
                                                    ,last-static-dep-results ,new-dep-results))
                               (cl-every #'identity
                                         (cl-mapcar #'eq
                                                    ,last-dynamic-dep-results
                                                    (mapcar #'sscell-get ,dynamic-deps))))
                    ,new-dep-results)))))
         (let ((,self (or ,reuse-cons (cons sscell--tag nil))))
           (setcdr ,self
                   (lambda (,instruction &optional ,arg)
                     (pcase-exhaustive ,instruction
                       (:valid?
                        (if (not (funcall ,cell-invalid-p))
                            t
                          (setq ,last-static-dep-results nil)
                          nil))
                       (:get
                        (when sscell--asking-sscell
                          (add-to-list 'sscell--new-dynamic-deps ,self))
                        (let ((sscell--asking-sscell ,self)
                              (sscell--new-dynamic-deps nil))
                          (when-let ((,new-dep-results (funcall ,cell-invalid-p)))
                            (setq ,last-static-dep-results  ,new-dep-results
                                  ,last-result              (funcall ,get-value)
                                  ,dynamic-deps             sscell--new-dynamic-deps
                                  ,last-dynamic-dep-results (mapcar #'sscell-get ,dynamic-deps))))
                        ,last-result))))
           ,self)))))

(defmacro sscell-make (static-deps rule)
  "Make an sscell.
STATIC-DEPS is a list of the static dependencies of the sscell.
A static dependency is either:
   EXPR
or
  (:test TESTFUN EXPR)

RULE is an expression to (re-)calculate the cell value."
  (declare (indent 1))
  `(sscell-make--1 ,static-deps ,rule nil))

(defun sscell-get (sscell)
  (cl-assert (sscellp sscell))
  (funcall (cdr sscell) :get))

(defun sscell-valid-p (sscell)
  (cl-assert (sscellp sscell))
  (funcall (cdr sscell) :valid?))

(defmacro sscell-set (sscell new-static-deps new-rule)
  `(sscell-make--1 ,new-static-deps ,new-rule ,sscell))

(defun sscell-set-value (sscell value)
  (sscell-set sscell () value)
  value)

(gv-define-simple-setter sscell-get sscell-set-value)

(defmacro sscell-let (bindings &rest body)
  (declare (indent 1) (debug fixme))
  (cl-callf2 mapcar
      (pcase-lambda (`(,var ,deps ,binding))
        (list (make-symbol (concat (symbol-name var) "-sscell"))
              var deps binding))
      bindings)
  `(let ,(mapcar
          (pcase-lambda (`(,helper-var ,_var ,deps ,binding))
            `(,helper-var (sscell-make ,deps ,binding)))
          bindings)
     (cl-symbol-macrolet
         ,(mapcar (pcase-lambda (`(,helper-var ,var ,_deps ,_binding))
                    `(,var (sscell-get ,helper-var)))
                  bindings)
       ,@body)))

(defmacro sscell-let* (bindings &rest body)
  (declare (indent 1) (debug fixme))
  (cl-reduce
   (lambda (expr binding) `(sscell-let (,binding) ,expr))
   (nreverse bindings)
   :initial-value (macroexp-progn body)))


(provide 'sscell)

;;; sscell.el ends here
