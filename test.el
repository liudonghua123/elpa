;;; test.el --- num3-mode tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Michal Nazarewicz <mina86@mina86.com>

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

;;; Code:

(require 'ert)
(require 'num3-mode)

(defun num3-mode-test (text want)
  "Runs `num3-mode` matcher on TEXT and checks its fontification.
WANT describes expected fontification.  It’s a copy of TEXT
except characters which are expected to use odd face are replaced
by underscore (‘_’) and those which use even face are replaced by
caret (‘^’).  For example:

    (num3-mode-test \"6.2831853071\"
                    \"6.^^^___^^^_\")"
  (let ((got (with-temp-buffer
               (insert text)
               (goto-char (point-min))
               (let ((num3-group-size 3) (num3-threshold 5))
                 (num3--matcher (point-max)))
               (buffer-string))))
    (dotimes (idx (length got))
      (when-let ((face (get-text-property idx 'face got)))
        (aset got idx (cond ((equal face '(num3-face-even)) ?^)
                            ((equal face '(num3-face-odd )) ?_)
                            (??)))))
  (should (equal want got))))

(ert-deftest num3-mode-decimal-integers ()
  ;; Won’t fontify unless at least five digits.
  (num3-mode-test "123 1234 12345 123456789"
                  "123 1234 ^^___ ___^^^___"))

(ert-deftest num3-mode-octal-integers ()
  ;; There’s actually no special handling for octal numbers.  The ‘Oo’
  ;; and ‘#o’ prefixes ignored.  The end result is that we group octal
  ;; numbers just like decimal numbers.
  (num3-mode-test "012 0123 01234 01234567"
                  "012 0123 ^^___ __^^^___")
  (num3-mode-test "0o123 0o1234 0o12345 0o12345671"
                  "0o123 0o1234 0o^^___ 0o__^^^___")
  (num3-mode-test "#o123 #o1234 #o12345 #o12345671"
                  "#o123 #o1234 #o^^___ #o__^^^___")
  (num3-mode-test "0O123 0O1234 0O12345 0O12345671"
                  "0O123 0O1234 0O^^___ 0O__^^^___")
  (num3-mode-test "#O123 #O1234 #O12345 #O12345671"
                  "#O123 #O1234 #O^^___ #O__^^^___"))

(ert-deftest num3-mode-hexadecimal-integers ()
  ;; Won’t fontify unless at least five digits.
  (num3-mode-test "0x1b3 0x1b3d 0x1b3d5 0x1B3D5F789"
                  "0x1b3 0x1b3d 0x^____ 0x_^^^^____")
  (num3-mode-test "#x1b3 #x1b3d #x1b3d5 #x1B3D5F789"
                  "#x1b3 #x1b3d #x^____ #x_^^^^____")
  (num3-mode-test "0X1b3 0X1b3d 0X1b3d5 0X1B3D5F789"
                  "0X1b3 0X1b3d 0X^____ 0X_^^^^____")
  (num3-mode-test "#X1b3 #X1b3d #X1b3d5 #X1B3D5F789"
                  "#X1b3 #X1b3d #X^____ #X_^^^^____")
  ;; Some hexadecimal numbers are recognised even without prefix.  For
  ;; that to happen, they must include a decimal digit.
  (num3-mode-test "ABCD5678 abcd5678 ABCDABCD"
                  "^^^^____ ^^^^____ ABCDABCD"))

(ert-deftest num3-mode-binary-integers ()
  (num3-mode-test "0b101 0b1010 0b10101 0b101010101"
                  "0b101 0b1010 0b^____ 0b_^^^^____")
  (num3-mode-test "#b101 #b1010 #b10101 #b101010101"
                  "#b101 #b1010 #b^____ #b_^^^^____")
  (num3-mode-test "0B101 0B1010 0B10101 0B101010101"
                  "0B101 0B1010 0B^____ 0B_^^^^____")
  (num3-mode-test "#B101 #B1010 #B10101 #B101010101"
                  "#B101 #B1010 #B^____ #B_^^^^____"))

(ert-deftest num3-mode-decimal-fractions ()
  (num3-mode-test "6.28 6.2831 6.28318 23456.28318 .12345"
                  "6.28 6.2831 6.___^^ ^^___.___^^ .___^^"))

(ert-deftest num3-mode-hexadecimal-fractions ()
  (num3-mode-test
   "0x6a.2a8p5 0x6a.2b1p5 0x6a.2abc8p5 0x23456.28318p5 0x.12345p5"
   "0x6a.2a8p5 0x6a.2b1p5 0x6a.____^p5 0x^____.____^p5 0x.____^p5")
  (num3-mode-test "0x.12345p+5 0x.12345p-5 0x.12345p+12345"
                  "0x.____^p+5 0x.____^p-5 0x.____^p+^^___")
  ;; Exponent is required in the notation.  Without it those are two
  ;; literals: a hexadecimal integer, a dot token and decimal integer.
  (num3-mode-test "0x6a.12345 0x23456.28318"
                  "0x6a.___^^ 0x^____.___^^"))

;;; test.el ends here
