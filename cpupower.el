;;; cpupower.el --- cpupower command interface  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Stephen Meister

;; Author: Stephen Meister
;; URL: https://gitlab.com/steve-emacs-stuff/cpupower-el
;; Version: 1.0.0
;; Keywords: hardware, cpupower, cpu, frequency-scaling

;; This program is free software: you can redistribute it and/or modify
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

;; If you have cpupower installed, this provides a very simple wrapper
;; to that program.  You'll need to configure your system such that
;; the current user can run cpupower (maybe as `sudo cpupower` from a
;; command line).  You can configure how cpupower is called by
;; customizing cpupower-cmd.
;;
;; This module interacts with cpupower via running it in a shell.
;;
;; The commands you'll probably want to use:
;; * cpupower-info
;;   - displays (briefly) the current cpupower information
;; * cpupower-set-governor
;;   - sets cpu governor for all cores.
;; * cpupower-helm-set-governor
;;   - sets cpu governor for all cores (uses helm)
;;
;; Less useful commands:
;; * cpupower-get-current-frequencies
;;   - returns a list of all cpu frequencies in KHz by core.
;; * cpupower-get-current-governors
;;   - returns a list of all cpu governors by core.

;;; Code:
;; pretty sure I don't need this, but?
(setq lexical-binding t)

(defcustom cpupower-cmd
  "sudo cpupower"
  "cpupower command, might need a 'sudo' infront of it and you
might need to enable this command for your user in /etc/sudoers"
  :type '(string)
  :group 'cpupower)

(defcustom cpupower-enable-logging
  nil
  "When non-nil, cpupower.el will log all commands it runs to the
messages buffer"
  :type '(boolean)
  :group 'cpupower)

(defconst cpupower--compatible-versions
  '("5.4")
  "Versions of cpupower which cpupower.el can work with.

It is possible that other versions of cpupower have the same
command line input/output structure.  If you are working with a
version of cpupower not listed here but you suspect is
compatible, extending this list with you version will all you to
test.")

(defvar cpupower--cache
  nil
  "Where cached information about cpupower will be put")

(defmacro cpupower--with-cache-slot (cache-slot-name deriving-sexp)
  "Get something from cache (cpupower--cache) or derive it and populate cache.

CACHE-SLOT-NAME is the key under which the result is stored.

DERIVING-SEXP is a single sexp which should return the value.  It
will be called if the value is not cached."
  (declare (indent 1))
  `(let ((cached-value (plist-get cpupower--cache ,cache-slot-name)))
     (if cached-value
         cached-value
       (let ((derived-value ,deriving-sexp))
         (setf (plist-get cpupower--cache ,cache-slot-name) derived-value)
         derived-value))))

(defun cpupower--get-version ()
  "Return the cpupower executable version or 'nil on failure.

Note: this function skips the safety check because the safety
check is composed of this function."
  (cpupower--with-cache-slot :version
    (let* ((output (cpupower--run "--version" t))
           (tokens (split-string output)))
      (when (string-equal (car tokens) "cpupower")
        (cadr tokens)))))

(defun cpupower--ensure-executable-valid ()
  "This function will cause an error if an invalid configuration is detected.

The cpupower command must be executable and must indicate an
acceptible version."
  (let ((is-valid (cpupower--with-cache-slot :valid
                    (cl-member (cpupower--get-version)
                               cpupower--compatible-versions
                               :test 'string-equal))))
    (unless is-valid
      (let ((version (cpupower--get-version)))
        (if (null version)
            (error "Unable to communicate with cpupower (shell command: \"%s\")"
                   cpupower-cmd)
          (error "Invalid cpu power version %s.  Must be one of: %s"
                 version (mapconcat 'identity cpupower--compatible-versions ", ")))))))

(defun cpupower--get-num-cpus ()
  "Return the number of CPUs on this system."
  (cpupower--with-cache-slot :num-cpus
    (length (cpupower-get-current-frequencies))))

(defun cpupower--get-available-governors ()
  "Get a list of all valid governors for this system.

@todo - this should probably find governors which work for _all_
cpus but currently it just finds _all_ governors."
  (cpupower--with-cache-slot :governors
    (seq-uniq
     (mapcan
      'split-string
      (cpupower--parse-find (cpupower--run "--cpu all frequency-info")
                            "available cpufreq governors:"
                            "\n")))))

(defun cpupower--run (subcommand &optional skip-safety-check)
  "Execute cpupower with SUBCOMMAND arguments return the output as a string.

When SKIP-SAFETY-CHECK is non-nil the cpupower configuration will
be checked before executing any commands."
  (unless skip-safety-check
    (cpupower--ensure-executable-valid))
  (with-temp-buffer
    (let ((command (format "%s %s" cpupower-cmd subcommand)))
      (when cpupower-enable-logging
        (message "cpupower.el cmd: %s" command))
      (shell-command command (current-buffer))
      (buffer-string))))

(defun cpupower--format-KHz (KHz)
  "Format KHZ (as KHz) for human eyes (probably translating to MHz or GHz)."
  (cond ((> KHz 1000000)
         (format "%.2fGHz" (/ KHz 1000000.0)))
        ((> KHz 1000)
         (format "%.0fMHz" (/ KHz 1000.0)))
        (t
         (format "%dKHz" KHz))))

(defun cpupower--parse-find (source pre-string &optional token-delimeter)
  "Find whatever is after PRE-STRING in SOURCE and return it.

This function assumes that tokens are space delimited.

Example:
source => \"this here and this one here too\"
pre-string => \"here\"

function returns: (\"and\" \"too\")
"
  (let ((token-delimeter (or token-delimeter " "))
        (pre-length (length pre-string))
        (found-idx (cl-search pre-string source))
        (output))
    (while found-idx
      (let* ((token-start (+ 1 found-idx pre-length))
             (token-end (cl-search token-delimeter source :start2 token-start)))
        (push (substring source token-start token-end) output)
        (setq found-idx (cl-search pre-string source :start2 token-end))))
    output))

(defun cpupower-info ()
  "Place current cpupower information into the message buffer.

This is indented as the primary way for humans to see this
information."
  (interactive)
  (let ((governors (seq-uniq (cpupower-get-current-governors) 'string-equal))
        (frequencies (mapcar 'cpupower--format-KHz (cpupower-get-current-frequencies))))
    (message "Governor: %s [ %s ] (version: %s)"
             (mapconcat 'identity governors ", ")
             (mapconcat 'identity frequencies ", ")
             (cpupower--get-version))))

(defun cpupower-set-governor (governor)
  "Set the governor on all CPUs to a given GOVERNOR by name (string)."
  (interactive "sGovernor: ")
  (let ((valid-governors (cpupower--get-available-governors)))
    (unless (member governor valid-governors)
      (error "Invalid governor: %s, must be one of %s" governor valid-governors))
    (cpupower--run (format "--cpu all frequency-set -g %s" governor))
    (cpupower-info)))

(defun cpupower-get-current-frequencies (&optional print-message)
  "Return a list of CPU frequencies in KHz

When called interactively (PRINT-MESSAGE will be true) it will
message the user with current CPU frequencies."
  (interactive "p")
  (let* ((output (cpupower--run "--cpu all frequency-info -f"))
         (frequencies (mapcar 'string-to-number
                              (cpupower--parse-find output "frequency:"))))
    (when print-message
      (message (format "CPU Frequencies: %s"
                       (mapconcat 'cpupower--format-KHz frequencies ", "))))
    frequencies))

(defun cpupower-get-current-governors (&optional print-message)
  "Return a list of CPU governors

When called interactively (PRINT-MESSAGE will be true) it will
message the user with current CPU governors"
  (interactive "p")
  (let* ((output (cpupower--run "--cpu all frequency-info -p"))
         (governors (mapcar (lambda (quoted)
                              (string-replace "\"" "" quoted))
                            (cpupower--parse-find output "governor"))))
    (when print-message
      (message (format "CPU Governors: %s"
                       (mapconcat 'identity
                                  (seq-uniq governors 'string-equal)
                                  ", "))))
    governors))

;; optional helm interface
(with-eval-after-load 'helm
  ;; Not entirely sure I should be doing this function declaring?
  (declare-function helm helm)
  (declare-function helm-make-source helm)

  (require 'helm)
  (defun cpupower-helm-set-governor ()
    "Set cpu governor using helm."
    (interactive) 
    (cpupower-set-governor
     (helm :sources (helm-make-source "cpu-governors" 'helm-source-sync
                      :candidates (cpupower--get-available-governors)) 
           :buffer "*helm set cpu governor*"))))

(provide 'cpupower)

;;; cpupower.el ends here
