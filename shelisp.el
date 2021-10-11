;;; shelisp.el --- execute elisp in shell          -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; Author: Michael R. Mauger <michael@mauger.com>
;; Version: 1.0.0
;; Package-Type: simple
;; Keywords: terminals, lisp, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Comint process (likely shell-mode) can write out Emacs Lisp
;; expressions and have them executed.

;; When the shell process writes out a string of the form:
;;   \e_#EMACS# elisp-expr \a
;;
;; Where, "elisp-expr" is a valid elisp expression.  The elisp
;; expression is executed as if you had invoked the function
;; within Emacs itself.  The elisp expression may include a call to
;; the function `f' which will expand the filename parameter into an
;; appropriate filename for Emacs using the appropriate Tramp prefix
;; if necessary.

;; This script also defines an Alist variable that creates shell
;; commands and the `printf'-style format to generate the full elisp
;; expression with command parameters substituted into the command.  A
;; function is placed in the `shell-mode-hook' to actually create the
;; shell functions and aliases to format the elisp expressions and
;; embed them in an escape sequence so that they are detected and
;; executed.

;; In most usage this mode merely allows you to type "e filename"
;; rather than "C-x C-f filename" which isn't much of a savings.
;; However, with this mode enabled, you can write shell scripts to
;; invoke Emacs Lisp functions.  But beware, the shell script will not
;; wait for completion of the elisp expression, nor return anything
;; back (see ToDo's below).

;; INSTALLATION

;; After installing this package from ELPA, you must add the following
;; to your Emacs initialization script:

;;   (add-hook 'shell-mode-hook #'shelisp-mode)
;;   (add-hook 'term-mode-hook #'shelisp-mode)

;;   ;; iff `vterm' package was installed
;;   (add-hook 'vterm-mode-hook #'shelisp-mode)

;; TO DOs:

;; * Provide a security feature that prompts the Emacs user to approve
;;   the execution of any elisp expressions submitted thru the shelisp
;;   escape sequence.

;; * Support for bash, ksh, and fish is provided (thank you for your
;;   motivation and effort, Eduardo Ochs <eduardoochs@gmail.com>);
;;   support for csh, tcsh, ksh, ash, dash, mosh, and sh is welcomed.
;;
;;   Support for non-Linux shells is left as an exercise for a
;;   masochistic hacker.

;; * Implement a wait for completion facility similar to `emacsclient'
;;   or the work done in `with-editor' with the "sleeping editor."
;;   That is, pause the shell activity with a long sleep, until C-c
;;   C-c or C-c C-k is typed in Emacs and the caller is awoken with a
;;   signal.

;; KNOWN BUGS

;; The simplistic implementation of the shell functions will not
;; properly handle filenames containing double quote characters (\")
;; nor backslashes (\\).  While this is an error, it does not
;; represent a significant limitation in the implementation.  The
;; caller can properly add backslashes to the filename string before
;; passing it to printf to generate the elisp expression.  In the end,
;; the purpose is to create a valid elisp expression string.

;;; Code:
(require 'cl-lib)
(require 'pp)
(require 'term)

(declare-function internal-default-process-filter "process.c" '(proc text))

;;;###autoload
(define-minor-mode shelisp-mode
  "Enable elisp expressions embedded in ANSI APC (Application
Program Control) escape sequences to be located and executed
while in a shell mode buffer."
  nil " ShElisp" nil
  (if shelisp-mode
      (shelisp--enable)
    (shelisp--disable)))

;;;###autoload
(defvar shelisp-debug nil
  "When non-nil, display messages showing the elisp expression.")

(defvar shelisp-shell nil
  "Identifies the shell scripting environment in use.

If nil, the shell is inferred from the from the `shell'
settings (`explicit-shell-file-name', the environment variable
`ESHELL', or `shell-file-name').")

(defvar-local shelisp--previous-process-filter nil
  "Previous value of process filter.  See `set-process-filter'.")

(defvar shelisp--wrapper-commands
  '((bash
     "unset -f shelisp_%1$s"
     "function shelisp_%1$s { printf '\\e_#EMACS# %2$s \\e\\\\' \"$@\" ; }"
     "alias %1$s=shelisp_%1$s")
    (dash
     "shelisp_%1$s () {; printf '\\e_#EMACS# %2$s \\e\\\\' \"$@\" ; }"
     "alias %1$s=shelisp_%1$s")
    (zsh
     "unfunction shelisp_%1$s >/dev/null 2>&1"
     "function shelisp_%1$s { printf '\\e_#EMACS# %2$s \\e\\\\' \"$@\" ; }"
     "alias %1$s=shelisp_%1$s")
    (fish
     "function %1$s"
     "printf '\\e_#EMACS# %2$s \\e\\\\' $argv"
     "end"))

  "Alist of shell commands necessary to make ShElisp work.

The key of the alist is either an atom that identifies the type
of shell (See `shelisp-shell' for defining the type of shell).

The value is a series of strings which will be sent to the shell.
Each string will be separated by the `shelisp--wrapper-separator'
string.  The assempled string is used as a specification for the
`format' function.  The first format parameter (\"%1$s\") is the
command to be defined with \"%1$s\" set to the command; the
second parameter (\"%2$s\") is the elisp expression to be
executed when the command is used.")

(defvar shelisp--wrapper-separator " ; "
  "String to separate commands sent to the shell.")

(defun shelisp--file-name (file)
  "Apply remote host in `default-directory' to FILE.

This is bound to the function `f' in the elisp expression handled
by ShElisp.  This permits absolute filenames on remote hosts to
be properly referenced."

  (if (and (file-name-absolute-p file)
	   (not (file-remote-p file)))
      (concat (file-remote-p default-directory) file)
    file))

(defun shelisp--result-as-string (result)
  "Return RESULT as a string.
If it already is a string, then just return it.  Otherwise,
convert it to a string."
  (cond ((null result)    "")
        ((stringp result) result)
        (:else            (pp-to-string result))))

(defun shelisp-exec-lisp (&optional str)
  "Detect escape sequence in STR to execute Emacs Lisp."
  (interactive)

  (when (and shelisp-mode str)
    (let* ((APC "\\(?:\e_\\|\x9f\\)")
	   (tag "#EMACS#")
	   (ST  "\\(?:[\a\x9c]\\|[\e][\\\\]\\)")
	   (cmd-re "\\(?:[^\a\x9c\e]\\|\e[^\\\\]\\)")
	   (apc-re (concat APC tag "\\(" cmd-re "*\\)" ST))
	   (case-fold-search nil)
	   cmd rep)

      ;; Look for APC escape sequences
      (while (string-match apc-re str)
        (setq cmd (match-string 1 str)
              rep "")
        ;; Trace, if requested
        (when shelisp-debug
          (message "shelisp> `%s'" cmd))

        ;; Replace the elisp expresssion with it's value
        ;;   if the value is nil, treat it as an empty string
        (setq rep (save-match-data
                    (save-excursion
                      (condition-case err
                          (shelisp--result-as-string
                           (eval `(cl-flet ((f (file) (shelisp--file-name file)))
	                            ,(read cmd))
                                 t))
                        ;; When an error occurs, replace with the error message
	                (error
	                 (format "shelisp: `%s': %S" cmd err)))))
              str (replace-match
                   (concat rep (unless (string-equal "" rep) "\n"))
                   t t str)))))
  str)

(defun shelisp--process-output-filter (proc str)
  "Insert STR into buffer owned by PROC after executing elisp."

  (funcall (or shelisp--previous-process-filter
               #'internal-default-process-filter)
           proc
           (shelisp-exec-lisp str)))

;;;###autoload
(defvar shelisp-commands (let ((cmds '(("e" .     "(find-file-other-window (f \"%s\"))")
                                       ("v" .     "(view-file-other-window (f \"%s\"))")
                                       ("dired" . "(dired (f \"%s\"))")
                                       ("ediff" . "(ediff (f \"%s\") (f \"%s\"))")
                                       ("man"   . "(man \"%s\")"))))
                           (when (locate-library "magit")
                             (push '("magit" . "(magit-status)") cmds))
                           (when (or (bound-and-true-p viper-mode)
                                     (bound-and-true-p evil-mode))
                             (push '("vim" . "(find-file-other-window (f \"%s\"))") cmds)
                             (push '("vi" . "(find-file-other-window (f \"%s\"))") cmds))
                           cmds)

  "Alist of shell commands and corresponding Lisp expressions.
Each entry in the alist consists of the shell alias to be set as the
command, and the `printf' style string to generate the elisp
expression to be executed.

If a parameter to the elisp expression is a filename, then we
need to be sure that proper filename parsing in context occurs.
We do this by passing filename parameters through the elisp
function `f'[1].  This function makes sure that filename has
proper Tramp prefixes if the shell session is remote.  So, rather
than just embedding the filename in the elisp expression, using
printf, with \"\\\"%s\\\"\", you use \\=`(f \\\"%s\\\")\\='.

[1] The `f' function is `cl-flet' bound for the shelisp
expression and cannot be used elsewhere.")

(defun shelisp-add-commands ()
  "Add Emacs Lisp to shell aliases."

  ;; Infer the shell
  (unless shelisp-shell
    (let ((sh (or explicit-shell-file-name
                  (getenv "ESHELL")
                  shell-file-name)))
      (when sh
        (setq-local shelisp-shell (intern (file-name-base sh))))))

  (when (and shelisp-mode shelisp-commands shelisp-shell
             (assoc shelisp-shell shelisp--wrapper-commands))
    (let ((proc (get-buffer-process (current-buffer))))
      (dolist (c shelisp-commands)
        (let ((cmd (car c))
              (expr (cdr c)))
          (process-send-string
           proc
           (apply #'format
                  (mapconcat #'identity
                             (append (cdr (assoc shelisp-shell
                                                 shelisp--wrapper-commands))
                                     '(""))
                             shelisp--wrapper-separator)
                  (list cmd expr)))))

      (process-send-string proc "\n"))))


(defun shelisp--enable ()
  "Enable `shelisp-mode' in the current buffer."

  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ((derived-mode-p 'comint-mode)
      ;; Parse elisp escape sequences
      (add-hook 'comint-preoutput-filter-functions
	        #'shelisp-exec-lisp 'append)
      (shelisp-add-commands))

     (proc
      (setq shelisp--previous-process-filter (process-filter proc))
      (set-process-filter proc #'shelisp--process-output-filter)
      (shelisp-add-commands))

     (:else
      (message"ShElisp is not active")))))

(defun shelisp--disable ()
  "Disable `shelisp-mode' in the current buffer."

  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ((derived-mode-p 'comint-mode)
      (remove-hook 'comint-preoutput-filter-functions
	           #'shelisp-exec-lisp))

     (proc
      (set-process-filter proc shelisp--previous-process-filter)
      (setq shelisp--previous-process-filter nil)))))

(provide 'shelisp)
;;; shelisp.el ends here
