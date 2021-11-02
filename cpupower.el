;;; cpupower.el --- cpupower command line interface  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Stephen Meister

;; Author: Stephen Meister
;; URL: https://gitlab.com/steve-emacs-stuff/cpupower-el
;; Version: 1.0.0
;; Keywords: cpupower

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

;;; Commentary

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

(defconst cpupower--compatible-versions
  '("5.4")
  "Versions of cpupower which cpupower.el can work with.")

(defvar cpupower--info
  nil
  "Where cached information about cpupower will be put")

(defmacro cpupower--with-cache-slot (cache-slot-name deriving-sexp)
  "Get something from cache (cpupower--info) or derive it and populate cache.

CACHE-SLOT-NAME is the key under which the result is stored.

DERIVING-SEXP is a single sexp which should return the value.  It
will be called if the value is not cached."
  (declare (indent 1))
  `(let ((cached-value (plist-get cpupower--info ,cache-slot-name)))
     (if cached-value
         cached-value
       (let ((derived-value ,deriving-sexp))
         (setf (plist-get cpupower--info ,cache-slot-name) derived-value)
         derived-value))))

(defun cpupower--get-version ()
  "Return the cpupower executable version or 'nil on failure"
  (cpupower--with-cache-slot :version
    (let* ((output (cpupower--run "--version"))
           (tokens (split-string output)))
      (when (string-equal (car tokens) "cpupower")
        (cadr tokens)))))

(defun cpupower--get-num-cpus ()
  "Return the number of CPUs on this system.

Done by cat-ing /proc/cpuinfo and counting lines with
\"processor\" in them.

TODO: do this in a less bad way?"
  (cpupower--with-cache-slot :num-cpus
    (let ((cpu-count 0))
      (with-temp-buffer
        (insert-file-contents "/proc/cpuinfo")
        (while (search-forward "processor" nil t)
          (cl-incf cpu-count)))
      cpu-count)))

(defun cpupower--get-available-governors ()
  "Get a list of all valid governors for this system.

@todo - this should be done using cpupower? not cat-ing some random file."
  (cpupower--with-cache-slot :governors
    (let ((governors-per-cpu))
      (cl-loop for cpu-num in (number-sequence 0 (cpupower--get-num-cpus))
               for cpu-governors-file = (format "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_available_governors" cpu-num)
               while (file-exists-p cpu-governors-file)
               do (push (split-string
                         (with-temp-buffer
                           (insert-file-contents cpu-governors-file)
                           (buffer-string)))
                        governors-per-cpu))
      (cl-loop with valid-governors = (car governors-per-cpu)
               for other-governor-set in (cdr governors-per-cpu)
               do (setq other-governor-set
                        (cl-intersection valid-governors other-governor-set))
               finally return valid-governors))))

(defun cpupower--run (subcommand)
  "Execute cpupower with SUBCOMMAND arguments return the output as a string."
  (with-temp-buffer
    (let ((command (format "%s %s" cpupower-cmd subcommand)))
      (message "running: %s" command)
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

(defun cpupower--parse-output (output-string pre-token)
  "Warning: bad idea - parse console output for tokens

Split OUTPUT-STRING on whitespace characters then scan through
all the strings for the PRE-TOKEN.  When PRE-TOKEN is found the
next string is accumulated for output.

Example:
output-string => \"this here and this one here too\"
pre-token => \"here\"

function returns: (\"and\" \"too\")

:("
  (cl-loop with output-tokens = nil
           with next-token-is-target = nil
           for token in (split-string output-string)
           if next-token-is-target
           do (progn
                (push token output-tokens)
                (setq next-token-is-target nil))
           else
           do (when (string-equal token pre-token)
                (setq next-token-is-target t))
           finally return output-tokens))

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
                              (cpupower--parse-output output "frequency:"))))
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
                            (cpupower--parse-output output "governor"))))
    (when print-message
      (message (format "CPU Governors: %s"
                       (mapconcat 'identity
                                  (seq-uniq governors 'string-equal)
                                  ", "))))
    governors))

(with-eval-after-load 'helm
  ;; Not entirely sure I should be doing this function declaring?
  (declare-function helm "helm")
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
