;;; firefox-javascript-repl.el --- Jack into Firefox -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 1.0.0

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

;; REPL into a new Firefox instance's JavaScript engine.

;; Installation:

;; M-x package-install RET firefox-javascript-repl RET

;; Usage:

;; M-x firefox-javascript-repl RET

;;; Code:
(defcustom firefox-javascript-repl-binary "firefox"
  "The name or full path of the firefox binary to run."
  :group 'external
  :type 'string)

(defun firefox-javascript-repl--create-profile-directory ()
  "Create a profile directory."
  (let ((profile-directory (make-temp-file "firefox-javascript-repl-" t)))
    (with-current-buffer
        (find-file-noselect (expand-file-name "user.js" profile-directory))
      (insert "user_pref(\"devtools.debugger.remote-enabled\", true);\n")
      (insert "user_pref(\"devtools.chrome.enabled\", true);\n")
      (insert "user_pref(\"devtools.debugger.prompt-connection\", false);\n")
      (save-buffer 0))
    profile-directory))

(defun firefox-javascript-repl ()
  "Run a new instance of Firefox in a new profile.
Set about:config values `devtools.debugger.remote-enabled' to
true, `devtools.chrome.enabled' to true,
`devtools.debugger.prompt-connection' to false, and pass
`-start-debugger-server'.  Firefox will listen on
localhost (127.0.0.1) TCP port 6000."
  (interactive)
  (when (not (process-status "firefox-javascript-repl"))
    (let* ((profile-directory
            (firefox-javascript-repl--create-profile-directory))
           (process-name (file-name-nondirectory profile-directory))
           (firefox-process
            (start-process "firefox-javascript-repl"
                           process-name
                           firefox-javascript-repl-binary
                           "about:blank"
                           "-profile" profile-directory
                           "-start-debugger-server")))
      (set-process-sentinel firefox-process
                            (lambda (process event)
                              (message "%S %S" process event)
                              (when (or (string= event "killed\n")
                                        (string= event "finished\n"))
                                (message
                                 "firefox-javascript-repl: %s %s; deleting %s"
                                 process (string-trim event) profile-directory)
                                (delete-directory profile-directory t))))
      nil)))

(defun firefox-javascript-repl-stop ()
  "Stop the Firefox process started by `firefox-javascript-repl'."
  (interactive)
  (kill-process "firefox-javascript-repl"))

(provide 'firefox-javascript-repl)

;;; firefox-javascript-repl.el ends here
