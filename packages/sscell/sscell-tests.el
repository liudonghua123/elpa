;;; sscell-tests --- Regression tests for sscell.el   -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>

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


(require 'ert)
(require 'cl-lib)
(require 'sscell)
(eval-when-compile (require 'subr-x))


;; Tests analogue to thunk-tests.el

(ert-deftest sscell-is-lazy-and-can-be-evaluated ()
  (let* (x  (sscell (sscell-make () (ignore (setq x t)))))
    (should (null x))
    (ignore (sscell-get sscell))
    (should x)))

(ert-deftest sscell-evaluation-is-cached ()
  (let* ((x 0)
         (sscell (sscell-make () (setq x (1+ x)))))
    (ignore (sscell-get sscell))
    (should (= x 1))
    (ignore (sscell-get sscell))
    (should (= x 1))))

(ert-deftest sscell-let-basic-test ()
  "Test whether bindings are established."
  (should (equal (sscell-let ((x () 1) (y () 2)) (+ x y)) 3)))

(ert-deftest sscell-let*-basic-test ()
  "Test whether bindings are established."
  (should (equal (sscell-let* ((x () 1) (y () (+ 1 x))) (+ x y)) 3)))

(ert-deftest sscell-let-bound-vars-can-be-set-test ()
  ;; Contrary to thunks this works...
  "Test whether setting a `sscell-let' bound variable works."
  (should
   (eq 80 (sscell-let ((x () 1))
            (let ((y 7))
              (setq x (+ x y))
              (* 10 x))))))

(ert-deftest sscell-let-laziness-test ()
  "Test laziness of `sscell-let'."
  (should
   (equal (let ((x-evalled nil)
                (y-evalled nil))
            (sscell-let ((x () (progn (setq x-evalled t) (+ 1 2)))
                         (y () (progn (setq y-evalled t) (+ 3 4))))
              (let ((evalled-y y))
                (list x-evalled y-evalled evalled-y))))
          (list nil t 7))))

(ert-deftest sscell-let*-laziness-test ()
  "Test laziness of `sscell-let*'."
  (should
   (equal (let ((x-evalled nil)
                (y-evalled nil)
                (z-evalled nil)
                (a-evalled nil))
            (sscell-let* ((x () (progn (setq x-evalled t) (+ 1 1)))
                          (y () (progn (setq y-evalled t) (+ x 1)))
                          (z () (progn (setq z-evalled t) (+ y 1)))
                          (a () (progn (setq a-evalled t) (+ z 1))))
              (let ((evalled-z z))
                (list x-evalled y-evalled z-evalled a-evalled evalled-z))))
          (list t t t nil 4))))

(ert-deftest sscell-let-bad-binding-test ()
  "Test whether a bad binding causes an error when expanding."
  (should-error (macroexpand '(sscell-let ((x () 1 1)) x)))
  (should-error (macroexpand '(sscell-let (27) x)))
  (should-error (macroexpand '(sscell-let x x))))


;; Tests for implicit dependencies

(ert-deftest sscell-implicit-dep-test-1 ()
  (let ((a (sscell-make () 10))
        (b (sscell-make () 20))
        (c (sscell-make () 40)))
    (let* ((cell1 (sscell-make () (+ (sscell-get a) (sscell-get b))))
           (cell2 (sscell-make ()
                    (let ((counter 0))
                      (while (< (sscell-get cell1) (sscell-get c))
                        (cl-incf counter)
                        (cl-incf (sscell-get a)))
                      counter))))
      (should (eq (sscell-get cell2) 10)))))

(ert-deftest sscell-implicit-dep-test-2 ()
  (let ((cells (cl-loop for i from 1 to 10 collect (sscell-make () nil))))
    (sscell-set-value (nth 0 cells) 1)
    (cl-maplist (lambda (rest) (when (cdr rest) (sscell-set (cadr rest) () (1+ (sscell-get (car rest))))))
                cells)
    (should (eq (sscell-get (car (last cells))) 10))))


(provide 'sscell-tests)
;;; sscell-tests.el ends here
