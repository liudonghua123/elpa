;;; ulisp-repl.el --- uLisp REPL -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (paredit "26"))

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

;; Interact with uLisp running on a target board over a serial connection.

;; Usage:

;; M-x ulisp-repl

;;; Code:
(require 'comint)
(require 'lisp-mode)
(require 'paredit)

(defcustom ulisp-repl-serial-device-path nil
  "The full path of the serial device this REPL should use."
  :group 'comm
  :type 'string)

(define-derived-mode ulisp-repl--mode comint-mode "uLisp"
  "Major mode for interacting with a uLisp target board."
  :syntax lisp-mode-syntax-table
  :interactive nil
  :after-hook
  (progn
    ;; FIXME: make-serial-process and join the comint to that process.
    (keymap-local-set "RET" 'comint-send-input)
    (setq-local comint-process-echoes t)
    (setq-local comint-prompt-regexp (concat "^[0-9]+> "))
    (when (boundp 'comint-indirect-setup-function)
      (setq-local comint-indirect-setup-function 'lisp-mode))
    (when (fboundp 'comint-indirect-buffer)
      (comint-indirect-buffer))
    (when (fboundp 'comint-fontify-input-mode)
      (comint-fontify-input-mode))
    (make-local-variable 'kill-buffer-hook)
    (when (fboundp 'comint--indirect-cleanup)
      (add-hook 'kill-buffer-hook 'comint--indirect-cleanup))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (let ((network (get-process "*ulisp-repl*")))
                  (when network (kill-process network)))))))

(defun ulisp--filter-files (files)
  "Return FILES but with . and .. entries removed."
  (let (result)
    (dolist (file files)
      (when (not (or (string-match "/\\.$" file)
                     (string-match "/\\.\\.$" file)))
        (push file result)))
    (nreverse result)))

(defun ulisp--select-serial-device (prefix)
  "Return the full path to a serial device.
For the meaning of PREFIX, see `ulisp-repl'."
  (if (or prefix (not ulisp-repl-serial-device-path))
      (let ((base "/dev/serial/by-id/"))
        (if (and (file-readable-p base) (file-executable-p base))
            (let* ((files (ulisp--filter-files (directory-files base t)))
                   (file (if (not (eq (length files) 1))
                             (completing-read
                              "uLisp serial port: " files nil nil base)
                           (car files))))
              (if (and (file-readable-p file) (file-readable-p file))
                  (customize-set-value 'ulisp-repl-serial-device-path file)
                (error "Failed to access %s" file)))
          (error "Failed to access directory %s" base)))
    ulisp-repl-serial-device-path))

(defun ulisp-repl (prefix)
  "Start a uLisp REPL.
If PREFIX is non-nil, always query for the device path.  If
PREFIX is nil, return the previously confiugred device path, or
if that is nil, query."
  (interactive "P")
  (if (get-buffer-process "*ulisp-repl*")
      (pop-to-buffer "*ulisp-repl*")
    (with-current-buffer (get-buffer-create "*ulisp-repl*")
      (let ((device (ulisp--select-serial-device prefix)))
        (comint-exec
         (current-buffer) "ulisp-serial" "cu" nil (list "-l" device))
        (ulisp-repl--mode)
        (paredit-mode)
        (keymap-local-unset "RET" t)
        (pop-to-buffer (current-buffer))))))

(provide 'ulisp-repl)

;;; ulisp-repl.el ends here
