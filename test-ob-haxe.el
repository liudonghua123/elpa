;;; test-ob-haxe.el --- tests for ob-haxe.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2021 Free Software Foundation, Inc.
;; Author: Ian Martins

;; This file is not part of GNU Emacs.

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
(if t (require 'org-test))      ; Don't fail compilation if `org-test' absent.
(require 'ob-core)
(defvar org-babel-temporary-directory ; from ob-core
  (if (boundp 'org-babel-temporary-directory)
    org-babel-temporary-directory
  (temporary-file-directory)))

(org-test-for-executable "haxe")
(org-test-for-executable "neko")
(org-test-for-executable "hl")

; simple tests

(ert-deftest ob-haxe/simple ()
  "Hello world program that writes output."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
Sys.print(42);
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-bracket ()
  "Hello world program that outputs an open square bracket."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
Sys.print(\"[42\");
#+end_src"
   (should (string= "[42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-quote ()
  "Hello world program that writes quotes."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
Sys.print(\"\\\"42\\\"\");
#+end_src"
   (should (string= "\"42\"" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-return-int ()
  "Hello world program that returns an int value.  Also tests
that ob-haxe defaults to scripting mode."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil silent
return 42;
#+end_src"
   (should (eq 42 (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-return-float ()
  "Hello world program that returns a float value."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results value silent
return 42.1;
#+end_src"
   (should (equal 42.1 (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-return-string ()
  "Hello world program that returns a string value."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results value silent
return \"forty two\";
#+end_src"
    (should (string= "forty two" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-return-int-neko ()
  "Hello world program that returns an int value.  Also tests
that ob-haxe defaults to scripting mode."
  (org-test-with-temp-text
      "#+begin_src haxe :target neko :dir 'nil silent
return 42;
#+end_src"
   (should (eq 42 (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-return-int-hl ()
  "Hello world program that returns an int value.  Also tests
that ob-haxe defaults to scripting mode."
  (org-test-with-temp-text
      "#+begin_src haxe :target hl :dir 'nil silent
return 42;
#+end_src"
   (should (eq 42 (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-main ()
  "Hello world program that defines a main function."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
public static function main() {
    Sys.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-two-methods ()
  "Hello world program with two methods and no class."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
public static function main() {
    Sys.print(foo());
}
public static function foo() {
    return 42;
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-no-main ()
  "Hello world program with no main method.  Babel adds a dummy one so it can run without error."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
public static function foo() {
    return 42;
}
#+end_src"
    (should (string= "success" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-main-args-array ()
  "Hello world program that defines a main function with the square brackets after `args'."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
public static function main() {
    Sys.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-main-whitespace ()
  "Hello world program that defines a main function with the square brackets after `args'."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
public
static
function
main
 (
)
{
    Sys.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-class ()
  "Hello world program that defines a class."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
class Simple {
    public static function main() {
        Sys.print(42);
    }
}
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-class-and-package ()
  "Hello world program that defines a class and package."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent
package pkg;
class Simple {
    public static function main() {
        Sys.print(42);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-class-attr ()
  "Hello world program with class header attribute."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent :classname Simple
public static function main() {
    Sys.print(42);
}
#+end_src"
   (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/simple-with-class-attr-with-package ()
  "Hello world program with class attr with package."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output silent :classname pkg.Simple
public static function main() {
    Sys.print(42);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))


;; var tests

(ert-deftest ob-haxe/integer-var ()
  "Read and write an integer variable."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=42 :results output silent
Sys.print(a);
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/var-with-main ()
  "Read and write an integer variable, with main function provided."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=42 :results output silent
public static function main() {
    Sys.print(a);
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/var-with-class ()
  "Read and write an integer variable, with class provided."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=42 :results output silent
class Main {
    public static function main() {
        Sys.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/var-with-class-and-package ()
  "Read and write an integer variable, with class and package provided."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=42 :results output silent
package pkg;
class Main {
    public static function main() {
        Sys.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/var-with-class-and-hanging-curlies ()
  "Read and write an integer variable, with class with hanging curlies."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=42 :results output silent
class Main
{
    public static function main()
    {
        Sys.print(a);
    }
}
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/two-vars ()
  "Read two integer variables, combine and write them."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=21 b=2 :results output silent
Sys.print(a*b);
#+end_src"
    (should (string= "42" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/string-var ()
  "Read and write a string variable."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=\"forty two\" :results output silent
Sys.print('$a, len=${a.length}');
#+end_src"
    (should (string= "forty two, len=9" (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/multiline-string-var ()
  "Haxe doesn't support multiline string literals, so this errors."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=\"forty\ntwo\" :results output silent
Sys.print(String.format(\"%s, len=%d\", a, a.length()));
#+end_src"
    (should-error (org-babel-execute-src-block)))
  :type 'error)

;; return array

(ert-deftest ob-haxe/return-vector-using-array ()
  "Return a vector using an array."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results value vector silent
return [[4], [2]];
#+end_src"
    (should (equal '((4) (2))
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/read-return-array ()
  "Read and return an array."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_list :results value silent
return [a[0][0], a[1][0]];
#+end_src

#+name: haxe_list
- forty
- two"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/read-return-array-with-package ()
  "Read and return an array with package."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_list :results value silent
package pkg;
return [a[0][0], a[1][0]];
#+end_src

#+name: haxe_list
- forty
- two"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/output-list-with-spaces ()
  "Return a vector."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :results output list raw silent
Sys.println(\"forty two\");
Sys.println(\"forty two\");
#+end_src"
    (should (equal "forty two\nforty two\n"
                   (org-babel-execute-src-block)))))

;; list vars

(ert-deftest ob-haxe/list-var ()
  "Read and write a list variable."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a='(\"forty\" \"two\") :results value silent
var b = a;
return b;
#+end_src"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/vector-var ()
  "Read and write a vector variable."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a='[\"forty\" \"two\"] :results value silent
var b = a;
return b;
#+end_src"
    (should (equal '("forty" "two")
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/matrix-var ()
  "Read and write matrix variable."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_matrix :results value silent
var b = [[a[0][0], a[1][0]],
         [a[0][1], a[1][1]]];
return b; // transpose
#+end_src

#+name: haxe_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal '((2 4) (1 2))
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/matrix-var-with-header ()
  "Read matrix variable and write it with header."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_matrix :results value table silent
var b = [[\"col1\", \"col2\"],
         null,
         [a[0][0], a[1][0]],
         [a[0][1], a[1][1]]];
return b; // transpose
#+end_src

#+name: haxe_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal '(("col1" "col2") hline (2 4) (1 2))
                   (org-babel-execute-src-block)))))

;; output table

(ert-deftest ob-haxe/output-table-with-header ()
  "Write a table that includes a header."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_matrix :results output raw table silent
Sys.println(\"|col1|col2|\");
Sys.println(\"|-\");
for (ii in 0...a.length) {
    for (jj in 0...a[0].length) {
        Sys.print('|${a[ii][jj]}');
    }
    Sys.println(\"\");
 }
#+end_src

#+name: haxe_matrix
| 2 | 1 |
| 4 | 2 |"
    (should (equal "|col1|col2|\n|-\n|2|1\n|4|2\n"
                   (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/inhomogeneous_table ()
  "Read and write an inhomogeneous table."
  (org-test-with-temp-text
      "#+begin_src haxe :dir 'nil :var a=haxe_table :results value silent
return [[a[0][0], a[0][1]*2],
        [a[1][0], a[1][1]*2]];
#+end_src

#+name: haxe_table
  | string | number |
  |--------+--------|
  | forty  |      2 |
  | two    |      1 |"
   (should (equal
            '(("forty" 4) ("two" 2))
            (org-babel-execute-src-block)))))

;; imports

(ert-deftest ob-haxe/import_library ()
  "Import a standard haxe library."
  (org-test-with-temp-text
      "#+begin_src haxe :results output silent :imports haxe.crypto.Base64 haxe.io.Bytes
  var encoded = Base64.encode(Bytes.ofString(\"42\"));
  var decoded = Base64.decode(encoded);
  Sys.print('encoded=$encoded, decoded=$decoded');
#+end_src"
   (should (string=
            "encoded=NDI=, decoded=42"
            (org-babel-execute-src-block)))))

(ert-deftest ob-haxe/import_library_inline ()
  "Import a standard haxe library."
  (org-test-with-temp-text
      "#+begin_src haxe :results output silent
  import haxe.crypto.Base64;
  import haxe.io.Bytes;
  var encoded = Base64.encode(Bytes.ofString(\"42\"));
  var decoded = Base64.decode(encoded);
  Sys.print('encoded=$encoded, decoded=$decoded');
#+end_src"
   (should (string=
            "encoded=NDI=, decoded=42"
            (org-babel-execute-src-block)))))

;; tangle

(ert-deftest ob-haxe/tangle ()
  "Tangle a source block."
  (org-test-with-temp-text-in-file
      "#+begin_src haxe :dir 'nil :tangle \"Tangle.hx\" :results value :classname Tangle
return \"tangled\";
#+end_src"
    (should
     (string=
      "class Tangle {
    public static function main() {
        return \"tangled\";
    }
}
"
      (unwind-protect
          (progn (org-babel-tangle)
                 (with-temp-buffer
                   (insert-file-contents "Tangle.hx")
                   (untabify (point-min) (point-max))
                   (buffer-string)))
        (delete-file "Tangle.hx"))))))

(ert-deftest ob-haxe/tangle-with-package ()
  "Tangle a source block."
  (org-test-with-temp-text-in-file
      "#+begin_src haxe :dir 'nil :tangle \"tangle/Tangle.hx\" :results value :classname tangle.Tangle
return \"tangled\";
#+end_src"
    (should
     (string=
      "package tangle;

class Tangle {
    public static function main() {
        return \"tangled\";
    }
}
"
      (unwind-protect
          (progn
            (make-directory "tangle")
            (org-babel-tangle)
            (with-temp-buffer
              (insert-file-contents "tangle/Tangle.hx")
              (untabify (point-min) (point-max))
              (buffer-string)))
        (delete-file "tangle/Tangle.hx")
        (delete-directory "tangle"))))))


;; specify output dir

(ert-deftest ob-haxe/simple-dir ()
  "Hello world program that writes output."
  (org-test-with-temp-text
      (format  "#+begin_src haxe :dir %s :results output silent
Sys.print(42);
#+end_src" org-babel-temporary-directory)
    (should (string=
             "42"
             (unwind-protect
                 (org-babel-execute-src-block)
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "Main.hx")))))))

(ert-deftest ob-haxe/simple-dir-with-package ()
  "Hello world program that writes output."
  (org-test-with-temp-text
      (format "#+begin_src haxe :dir %s :results output silent
package pkg;

class Main {
    public static function main() {
      Sys.print(42);
    }
}
#+end_src" org-babel-temporary-directory)
    (should (string=
             "42"
             (unwind-protect
                 (org-babel-execute-src-block)
               (delete-file (concat (file-name-as-directory org-babel-temporary-directory)
                                    "pkg/Main.hx"))
               (delete-directory (concat (file-name-as-directory org-babel-temporary-directory)
                                         "pkg")))))))


;;; test-ob-haxe.el ends here
