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

(defun fjrepl--message (format &rest arguments)
  "Print to *Messages* a package herald and FORMAT.
ARGUMENTS will be used for FORMAT, like `messages'."
  (apply #'message (concat "firefox-javascript-repl: " format) arguments))

(defun fjrepl--error (message)
  "Throw an error with a package herald and MESSAGE."
  (error "firefox-javascript-repl: %s" message))

(defun fjrepl--create-profile-directory ()
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

(defun fjrepl--parse-prior-message (buffer)
  "Parse the JSON message before in BUFFER."
  (with-current-buffer buffer
    (when (re-search-backward "[0-9]+:{" nil t)
      (forward-char 2)
      (let ((start (point)))
        (forward-sexp)
        (prog1
            (json-parse-string (buffer-substring start (point)))
          (backward-sexp)))))  )

(defun fjrepl--get-result (buffer depth &rest arguments)
  "Return a field from BUFFER.
BUFFER holds Firefox's Remote Debug Protocol response messages.
DEPTH is non-nil if ARGUMENTS represent a decending path through
the data structure.  DEPTH is nil if ARGUMENTS should match
fields at the current depth level.  Search from the most recent
to the least recent messages.  Return the filtered value based on
the matching arguments in the hierarchy.  ARGUMENTS is a list of
numbers and strings.  A number is used to pick out an element in
a JSON list, a string is used to pick out an element from a JSON
map."
  (with-current-buffer buffer
    (goto-char (point-max))
    (unless (bolp) (insert "\n")) ; avoid one long line
    (let (result)
      (catch 'match
        (while (setq result (fjrepl--parse-prior-message buffer))
          (catch 'fail
            (dolist (argument arguments) ; check that all arguments match
              (cond ((numberp argument)
                     (setq result (when (vectorp result)
                                    (elt result argument))))
                    ((stringp argument)
                     (if depth
                         (setq result (when (hash-table-p result)
                                        (gethash argument result)))
                       (setq result (and (gethash argument result)
                                         result)))))
              (unless result (throw 'fail nil))))
          (when result (throw 'match result)))))))

(defun fjrepl--accept-input (&optional network)
  "Wait up to ten sections to see if packets are received.
If NETWORK is non-nil, check for output from it.  Otherwise check
for output from any processes on which Emacs is waiting.."
  (dotimes (_count 20)
    (accept-process-output network 0.1)))

(defun fjrepl--send-string (network message)
  "Send to NETWORK a MESSAGE then wait for the response."
  (process-send-string network message)
  (fjrepl--accept-input))

(defun fjrepl--handle-done (name buffer network)
  "Process the startListeners response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (fjrepl--message "Ready for asynchronous JavaScript evaluation %s %s %s"
                   name buffer network))

(defun fjrepl--handle-console (name buffer network)
  "Process the startListeners response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (fjrepl--message "Handling console message")
  (when (get-buffer buffer)
    (let ((result (fjrepl--get-result buffer nil "startedListeners" "from")))
      (when result
        (let ((console-actor (gethash "from" result)))
          (fjrepl--message "Ready for asynchronous JavaScript evaluation on %s"
                           console-actor)))
      (run-at-time (unless result 1) nil
                   (if result
                       'fjrepl--handle-done 'fjrepl--handle-console)
                   name buffer network))))

(defun fjrepl--format-message (format &rest arguments)
  "Format a message for the Firefox Remote Debug Protocol wire.
FORMAT is a format string taken by `format', and ARGUMENTS are
the arguments for the format string."
  (let ((message (apply 'format format arguments)))
    (format "%d:%s" (length message) message)))

(defun fjrepl--handle-target (name buffer network)
  "Process the getTarget response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (fjrepl--message "Handling target message")
  (when (get-buffer buffer)
    (let ((console-actor (fjrepl--get-result buffer t "frame" "consoleActor")))
      (when console-actor
        (process-send-string network
                             (fjrepl--format-message
                              (concat
                               "{\"type\":\"startListeners\","
                               "\"to\":\"%s\","
                               "\"listeners\":[\"eventListener\"]}")
                              console-actor)))
      (run-at-time (unless console-actor 1) nil
                   (if console-actor
                       'fjrepl--handle-console 'fjrepl--handle-target)
                   name buffer network))))

(defun fjrepl--handle-tab (name buffer network)
  "Process the listTabs response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (fjrepl--message "Handling tab message")
  (when (get-buffer buffer)
    (let ((actor (fjrepl--get-result buffer t "tabs" 0 "actor")))
      (when actor
        (process-send-string network
                             (fjrepl--format-message
                              "{\"type\":\"getTarget\",\"to\":\"%s\"}" actor)))
      (run-at-time (unless actor 1) nil
                   (if actor 'fjrepl--handle-target 'fjrepl--handle-tab)
                   name buffer network))))

(defun fjrepl--handle-first (name buffer network)
  "Connect to Firefox process and process the first message from Firefox.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (fjrepl--message "Handling first message")
  (let ((network network))
    (unwind-protect
        (setq network (ignore-errors
                        (open-network-stream name buffer "127.0.0.1" 6000)))
      (let ((nextp
             (and network (fjrepl--get-result buffer t "applicationType"))))
        (when nextp
          (process-send-string
           network "31:{\"to\":\"root\",\"type\":\"listTabs\"}"))
        (run-at-time (unless nextp 1) nil
                     (if nextp 'fjrepl--handle-tab 'fjrepl--handle-first)
                     name buffer network)))))

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
            (fjrepl--create-profile-directory))
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
                                (fjrepl--message
                                 "%s %s; deleting %s"
                                 process (string-trim event) profile-directory)
                                (ignore-errors
                                  (kill-buffer network-buffer-name))
                                (kill-buffer process-buffer-name)
                                (delete-directory profile-directory t))))
      (fjrepl--handle-first network-name network-buffer-name nil)
      nil)))

(defun firefox-javascript-repl-stop ()
  "Stop the Firefox process started by `firefox-javascript-repl'."
  (interactive)
  (kill-process "firefox-javascript-repl"))

(provide 'firefox-javascript-repl)

;;; firefox-javascript-repl.el ends here
