;;; ob-haxe.el --- org-babel functions for haxe evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; Author: Ian Martins <ianxm@jhu.edu>
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating haxe source code.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("haxe" . "hx"))

(defvar org-babel-temporary-directory) ; from ob-core

(defvar org-babel-default-header-args:haxe '()
  "Default header args for haxe source blocks.")

(defconst org-babel-header-args:haxe '((imports . :any))
  "Haxe-specific header arguments.")

(defcustom org-babel-neko-command "neko"
  "Name of the neko command.
May be either a command in the path, like \"neko\" or the full
path to the executable, like \"/usr/local/bin/neko\".  Double
quotes must be escaped.  This is run in a shell."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-hashlink-command "hl"
  "Name of the hashlink command.
May be either a command in the path, like \"hl\" or the full path
to the executable, like \"/usr/local/bin/hl\".  Double quotes
must be escaped.  This is run in a shell."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-haxe-compiler "haxe"
  "Name of the haxe compiler.
May be either a command in the path like \"haxe\", or an absolute
file name, like \"/usr/local/bin/haxe\".  This is used in a shell
command, so parameters may be used, like \"haxe --verbose\"."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-haxe-hline-to "null"
  "Replace hlines in incoming tables with this when translating to haxe."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-haxe-null-to 'hline
  "Replace `null' in haxe tables with this before returning."
  :group 'org-babel
  :type 'symbol)

(defconst org-babel-haxe--package-re (rx line-start (0+ space) "package"
                                         (1+ space) (group (1+ (in alnum ?_ ?.))) ; capture the package name
                                         (0+ space) ?\; line-end)
  "Regexp for the package statement.")
(defconst org-babel-haxe--imports-re (rx line-start (0+ space) "import"
                                         (1+ space) (group (1+ (in alnum ?_ ?.))) ; capture the fully qualified class name
                                         (0+ space) ?\; line-end)
  "Regexp for import statements.")
(defconst org-babel-haxe--class-re (rx line-start (0+ space) "class" (1+ space)
                                       (group (1+ (in alnum ?_))) ; capture the class name
                                       (0+ space) ?{)
  "Regexp for the class declaration.")
(defconst org-babel-haxe--main-re (rx line-start (0+ space)
                                      (opt "public" (1+ space))
                                      "static"
                                      (1+ space) "function"
                                      (1+ space) "main"
                                      (0+ space) ?\(
                                      (0+ space) ?\)
                                      (0+ space) ?{)
  "Regexp for the main method declaration.")
(defconst org-babel-haxe--any-method-re (rx line-start
                                            (0+ space) (opt (seq (1+ alnum) (1+ space)))   ; visibility
                                            (opt (seq "static" (1+ space)))                ; binding
                                            "function"                                     ; function
                                            (1+ space) (1+ (in alnum ?_))                  ; method name
                                            (0+ space) ?\(
                                            (0+ space) (0+ (in alnum ?_ ?\[ ?\] ?, space)) ; params
                                            (0+ space) ?\)
                                            (0+ space) (opt ?: (0+ space) (1+ (in alnum ?_ ?\[ ?\]))) ; return type
                                            (0+ space) ?{)
  "Regexp for any method.")
(defconst org-babel-haxe--result-wrapper "
    public static function main() {
        var output = File.write(\"%s\");
        output.writeString(haxe.Json.stringify(_main()));
        output.close();
    }\n"
  "Code to inject into a class so that we can capture the value it returns.
This implementation was inspired by ob-python, although not as
elegant.  This modified the source block to write out the value
it wants to return to a temporary file so that ob-haxe can read
it back.  The name of the temporary file to write must be
replaced in this string.")

(defun org-babel-execute:haxe (body params)
  "Execute a haxe source block with BODY code and PARAMS params."
  (let* (;; allow header overrides
         (org-babel-haxe-compiler
          (or (cdr (assq :haxe params))
              org-babel-haxe-compiler))
         (org-babel-neko-command
          (or (cdr (assq :neko params))
              org-babel-neko-command))
         (org-babel-hashlink-command
          (or (cdr (assq :hashlink params))
              org-babel-hashlink-command))
         ;; if true, run from babel temp directory
         (run-from-temp (not (alist-get :dir params)))
         ;; class and package
         (fullclassname (or (cdr (assq :classname params))
                            (org-babel-haxe-find-classname body)))
         ;; just the class name
         (classname (car (last (split-string fullclassname "\\."))))
         ;; just the package name
         (packagename (if (string-match-p "\\." fullclassname)
                          (file-name-base fullclassname)))
         ;; the base dir that contains the top level package dir
         (basedir (file-name-as-directory (if run-from-temp
                                              (if (file-remote-p default-directory)
                                                  (concat
                                                   (file-remote-p default-directory)
                                                   org-babel-remote-temporary-directory)
                                                org-babel-temporary-directory)
                                            default-directory)))
         (basedir-processed (org-babel-process-file-name basedir 'noquote))
         ;; the dir to write the source file
         (packagedir (if (and (not run-from-temp) packagename)
                         (file-name-as-directory
                          (concat basedir (replace-regexp-in-string "\\." "/" packagename)))
                       basedir))
         ;; runtime flags
         (cmdline (or (cdr (assq :cmdline params)) ""))
         ;; compilation target
         (target-name (cdr (assq :target params)))
         (target (pcase target-name
                   ("neko"
                    (format "-neko %smain.n -cmd \"%s %smain.n %s\""
                            basedir-processed org-babel-neko-command basedir-processed cmdline))
                   ("hashlink"
                    (format "-hl %smain.hl -cmd \"%s %smain.hl %s\""
                            basedir-processed org-babel-hashlink-command basedir-processed cmdline))
                   (_
                    (if (> (length cmdline) 0)
                        (error "Cmdline args not allowed for interp target")
                      "--interp"))))
         ;; the command to compile and run
         (cmd (concat org-babel-haxe-compiler
                      " -p " basedir
                      " -main " (if run-from-temp classname fullclassname)
                      " " target))
         ;; header args for result processing
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (result-file (and (eq result-type 'value)
                           (org-babel-temp-file "haxe-")))
         ;; the expanded body of the source block
         (full-body (org-babel-expand-body:haxe body params)))

    ;; created package-name directories if missing
    (unless (or (not packagedir) (file-exists-p packagedir))
      (make-directory packagedir 'parents))

    ;; write the source file
    (setq full-body (org-babel-haxe--expand-for-evaluation
                     full-body run-from-temp result-type result-file))
    (with-temp-file (concat (file-name-as-directory packagedir)
                            classname ".hx")
      (insert full-body))

    ;; compile, run, process result
    (org-babel-reassemble-table
     (org-babel-haxe-evaluate cmd result-type result-params result-file)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

;; helper functions

(defun org-babel-haxe-find-classname (body)
  "Try to find fully qualified class name in BODY.
Look through BODY for the package and class.  If found, put them
together into a fully qualified class name and return.  Else just
return class name.  If that isn't found either, default to Main."
  (let ((package (if (string-match org-babel-haxe--package-re body)
                     (match-string 1 body)))
        (class (if (string-match org-babel-haxe--class-re body)
                   (match-string 1 body))))
    (or (and package class (concat package "." class))
        (and class class)
        (and package (concat package ".Main"))
        "Main")))

(defun org-babel-haxe--expand-for-evaluation (body suppress-package-p result-type result-file)
  "Expand source block for evaluation.
In order to return a value we have to add a __toString method.
In order to prevent classes without main methods from erroring we
add a dummy main method if one is not provided.  These
manipulations are done outside of `org-babel--expand-body' so
that they are hidden from tangles.

BODY is the file content before instrumentation.

SUPPRESS-PACKAGE-P if true, suppress the package statement.

RESULT-TYPE is taken from params.

RESULT-FILE is the temp file to write the result."
  (with-temp-buffer
    (insert body)

    ;; suppress package statement
    (goto-char (point-min))
    (when (and suppress-package-p
               (re-search-forward org-babel-haxe--package-re nil t))
      (replace-match ""))

    ;; add a dummy main method if needed
    (goto-char (point-min))
    (when (not (re-search-forward org-babel-haxe--main-re nil t))
      (org-babel-haxe--move-past org-babel-haxe--class-re)
      (insert "\n    public static function main() {
        Sys.print(\"success\");
    }\n\n"))

    ;; special handling to return value
    (when (eq result-type 'value)
      (goto-char (point-min))
      (org-babel-haxe--move-past org-babel-haxe--class-re)
      (insert (format org-babel-haxe--result-wrapper
                      (org-babel-process-file-name result-file 'noquote)))
      (search-forward "public static function main(") ; rename existing main
      (replace-match "public static function _main("))

    ;; add imports
    (org-babel-haxe--import-maybe "sys.io" "File")

    (buffer-string)))

(defun org-babel-haxe--move-past (re)
  "Move point past the first occurrence of the given regexp RE."
  (while (re-search-forward re nil t)
    (goto-char (1+ (match-end 0)))))

(defun org-babel-haxe--import-maybe (package class)
  "Import from PACKAGE the given CLASS if it is used and not already imported."
  (let (class-found import-found)
    (goto-char (point-min))
    (setq class-found (re-search-forward class nil t))
    (goto-char (point-min))
    (setq import-found (re-search-forward (concat "^import .*" package ".*" class ";") nil t))
    (when (and class-found (not import-found))
      (org-babel-haxe--move-past org-babel-haxe--package-re)
      (insert (concat "import " package "." class ";\n")))))

(defun org-babel-expand-body:haxe (body params)
  "Expand BODY with PARAMS.
BODY could be a few statements, or could include a full class
definition specifying package, imports, and class.  Because we
allow this flexibility in what the source block can contain, it
is simplest to expand the code block from the inside out."
  (let* ((fullclassname (or (cdr (assq :classname params)) ; class and package
                            (org-babel-haxe-find-classname body)))
         (classname (car (last (split-string fullclassname "\\.")))) ; just class name
         (packagename (if (string-match-p "\\." fullclassname)       ; just package name
                          (file-name-base fullclassname)))
         (var-lines (org-babel-variable-assignments:haxe params))
         (imports-val (assq :imports params))
         (imports (if imports-val
                      (split-string (org-babel-read (cdr imports-val) nil) " ")
                    nil)))
    (with-temp-buffer
      (insert body)

      ;; wrap main.  If there are methods defined, but no main method
      ;; and no class, wrap everything in a generic main method.
      (goto-char (point-min))
      (when (and (not (re-search-forward org-babel-haxe--main-re nil t))
                 (not (re-search-forward org-babel-haxe--any-method-re nil t)))
        (org-babel-haxe--move-past org-babel-haxe--package-re) ; if package is defined, move past it
        (org-babel-haxe--move-past org-babel-haxe--imports-re) ; if imports are defined, move past them
        (insert "public static function main() {\n")
        (indent-code-rigidly (point) (point-max) 4)
        (goto-char (point-max))
        (insert "\n}"))

      ;; wrap class.  If there's no class, wrap everything in a
      ;; generic class.
      (goto-char (point-min))
      (when (not (re-search-forward org-babel-haxe--class-re nil t))
        (org-babel-haxe--move-past org-babel-haxe--package-re) ; if package is defined, move past it
        (org-babel-haxe--move-past org-babel-haxe--imports-re) ; if imports are defined, move past them
        (insert (concat "\nclass " (file-name-base classname) " {\n"))
        (indent-code-rigidly (point) (point-max) 4)
        (goto-char (point-max))
        (insert "\n}"))
      (goto-char (point-min))

      ;; insert variables from source block headers
      (when var-lines
        (goto-char (point-min))
        (org-babel-haxe--move-past org-babel-haxe--class-re)   ; move inside class
        (insert (mapconcat #'identity var-lines "\n"))
        (insert "\n"))

      ;; add imports from source block headers
      (when imports
        (goto-char (point-min))
        (org-babel-haxe--move-past org-babel-haxe--package-re) ; if package is defined, move past it
        (insert (mapconcat (lambda (package) (concat "import " package ";")) imports "\n") "\n"))

      ;; add package at the top
      (goto-char (point-min))
      (when (and packagename (not (re-search-forward org-babel-haxe--package-re nil t)))
        (insert (concat "package " packagename ";\n")))

      ;; return expanded body
      (buffer-string))))

(defun org-babel-variable-assignments:haxe (params)
  "Return a list of haxe statements assigning the block's variables.
variables are contained in PARAMS."
  (mapcar
   (lambda (pair)
     (format "    static var %s %s = %s;"
             (car pair)                                ; name
             (if (and (sequencep (cdr pair))           ; type
                      (not (stringp (cdr pair))))
                 ":Array<Dynamic> " "")
             (org-babel-haxe-var-to-haxe (cdr pair)))) ; value
   (org-babel--get-vars params)))

(defun org-babel-haxe-var-to-haxe (var)
  "Convert an elisp value to a haxe variable.
Convert an elisp value, VAR, into a string of haxe source code
specifying a variable of the same value."
  (cond ((and (sequencep var)
              (not (stringp var)))
         (concat "[" (mapconcat #'org-babel-haxe-var-to-haxe var ", ") "]"))
        ((equal var 'hline)
         org-babel-haxe-hline-to)
        (t
         (format "%S" (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-haxe-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or vector, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq 'null el)
                                 org-babel-haxe-null-to
                               el))
                res)
      res)))

(defun org-babel-haxe-evaluate (cmd result-type result-params result-file)
  "Evaluate using an external haxe process.
CMD the command to execute.

If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value
returned by the source block, as elisp.

RESULT-PARAMS input params used to format the reponse.

RESULT-FILE filename of the tempfile to store the returned value in
for `value' RESULT-TYPE.  Not used for `output' RESULT-TYPE."
  (let ((raw (pcase result-type
               (`output (org-babel-eval cmd ""))
               (`value (org-babel-eval cmd "")
                       (org-babel-eval-read-file result-file)))))
    (org-babel-result-cond result-params raw
      (org-babel-haxe-table-or-string raw))))

(provide 'ob-haxe)

;;; ob-haxe.el ends here
