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

(defun firefox-javascript-repl--message (format &rest arguments)
  "Print to *Messages* a package herald and FORMAT.
ARGUMENTS will be used for FORMAT, like `messages'."
  (apply #'message (concat "firefox-javascript-repl: " format) arguments))

(defun firefox-javascript-repl--error (message)
  "Throw an error with a package herald and MESSAGE."
  (error "firefox-javascript-repl: %s" message))

(defun firefox-javascript-repl--create-profile-directory ()
  "Create a profile directory."
  (let ((profile-directory (make-temp-file "firefox-javascript-repl-" t)))
    (with-current-buffer
        (find-file-noselect (expand-file-name "user.js" profile-directory))
      (insert "user_pref(\"devtools.debugger.remote-enabled\", true);\n")
      (insert "user_pref(\"devtools.chrome.enabled\", true);\n")
      (insert "user_pref(\"devtools.debugger.prompt-connection\", false);\n")
      (save-buffer 0)
      (kill-buffer))
    profile-directory))

(defun firefox-javascript-repl--get-first-tab-actor (network-buffer)
  "Get the actor of the first browser tab.
NETWORK-BUFFER is where Firefox responses go."
  (gethash
   "actor"
   (elt
    (gethash
     "tabs"
     (with-current-buffer network-buffer
       (firefox-javascript-repl--message "BUF: %S" (buffer-string))
       (goto-char (point-max))
       (unless (bolp)
         (insert "\n"))
       (search-backward "{\"tabs\":")
       (let ((start (point)))
         (forward-sexp)
         (prog1
             (json-parse-string (buffer-substring start (point)))
           (goto-char (point-max))))))
    0)))

(defun firefox-javascript-repl--accept-input (network)
  "Wait up to ten sections to see if packets are received from NETWORK."
  (unless (accept-process-output network 10 0 t)
    (firefox-javascript-repl--error
     "Failed to receive network packets from Firefox")))

(defun firefox-javascript-repl--send-string (network message)
  "Send to NETWORK a MESSAGE then wait for the response."
  (process-send-string network message)
  (firefox-javascript-repl--accept-input network))

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
           (temporary-name (file-name-nondirectory profile-directory))
           (process-name (concat "out-" temporary-name))
           (process-buffer-name (concat "*" process-name "*"))
           (network-name (concat "net-" temporary-name))
           (network-buffer-name (concat "*" network-name "*"))
           (firefox-process
            (start-process "firefox-javascript-repl"
                           process-buffer-name
                           firefox-javascript-repl-binary
                           "about:blank"
                           "-profile" profile-directory
                           "-start-debugger-server")))
      (set-process-sentinel firefox-process
                            (lambda (process event)
                              (when (or (string= event "killed\n")
                                        (string= event "finished\n"))
                                (firefox-javascript-repl--message
                                 "%s %s; deleting %s"
                                 process (string-trim event) profile-directory)
                                (ignore-errors (kill-buffer network-buffer-name))
                                (kill-buffer process-buffer-name)
                                (delete-directory profile-directory t))))
      (let (network)
        (catch 'connected
          (dotimes (_count 50)
            (when
                (setq network
                      (ignore-errors
                        (open-network-stream process-name network-buffer-name
                                             "127.0.0.1" 6000)))
              (throw 'connected network))
            (sit-for 2))
          (firefox-javascript-repl--error
           "Failed to connect to Firefox network port"))
        (firefox-javascript-repl--accept-input network)
        (firefox-javascript-repl--message "Established Firefox connection")
        (firefox-javascript-repl--send-string
         network "31:{\"to\":\"root\",\"type\":\"listTabs\"}")
        (let ((actor (firefox-javascript-repl--get-first-tab-actor
                      network-buffer-name)))
          (message "Actor: %s" actor)))
      nil)))

(defun firefox-javascript-repl-stop ()
  "Stop the Firefox process started by `firefox-javascript-repl'."
  (interactive)
  (kill-process "firefox-javascript-repl"))

(provide 'firefox-javascript-repl)

;;; firefox-javascript-repl.el ends here
