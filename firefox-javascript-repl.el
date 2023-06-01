;;; firefox-javascript-repl.el --- Jack into Firefox -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 0.9.3
;; Package-Requires: ((emacs "26.1"))

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

;; Usage:

;; Start the REPL:

;; M-x firefox-javascript-repl RET

;; To stop the REPL, kill the *firefox-javascript-debugger* buffer:

;; C-x k RET yes RET

;; or quit Firefox from within Firefox.

;; Description:

;; REPL into a new Firefox instance's JavaScript engine.  A new
;; throwaway Firefox profile directory is created before each run, so
;; you won't need to modify your existing profiles.  This mode takes
;; care of starting the new Firefox process in debugging mode, which
;; may be tedious to do by hand.

;; This `comint' mode is barebones and unstructured, meant for quick
;; JavaScript experiments.  On newer versions of Emacs with
;; `comint-indirect-buffer' support, syntax highlighting happens on
;; the current statement.

;; Paste each statement from `example.js' into the REPL to try it out.

;; For projects you should probably use `dap-mode' and `lsp-mode'
;; instead.

;; Only Firefox and Firefox-derivative browsers will ever be supported
;; (unless someone sends a really convincing patch).  I promise to
;; attempt to stive to keep this working with at least the
;; greenest-of-evergreen Firefox and Firefox ESR versions (see
;; Compatibility).  My sense is that the Firefox Remote Debugging
;; Protocol is less of a moving target than it used to be.  Emacs
;; versions back to 26.1 (or earlier if anyone can test on Emacs <
;; 26.1) will be supported.

;; Wouldn't it be great (for other people) to turn this into a full
;; SLIME analog for JavaScript (patches accepted)?  I tried `jsSlime'
;; (https://github.com/segv/jss) but its most recent update is ten
;; years old and the Firefox Remote Debugging Protocol has changed too
;; much.

;; The function `fjrepl--extract-result' could do a way better job of
;; getting results but I find it OK for little experiments.  If I need
;; more information I submit a more precise JavaScript statement.
;; Syntax errors currently fail silently.

;; Installation:

;; M-x package-install RET firefox-javascript-repl RET

;; Compatibility:

;; ╔════════════╦══════════════╦══════════╦══════════════════════╗
;; ║  Test Date ║ Architecture ║ Browser  ║ Version              ║
;; ╠════════════╬══════════════╬══════════╬══════════════════════╣
;; ║ 2023-05-26 ║ ppc64le      ║ Firefox  ║ 102.11.0esr (64-bit) ║
;; ║ 2023-05-26 ║ x86-64       ║ Firefox  ║ 113.0.2 (64-bit)     ║
;; ║ 2023-05-26 ║ x86-64       ║ Abrowser ║ 111.0.1 (64-bit)     ║
;; ╙────────────╨──────────────╨──────────╨──────────────────────╜

;; Acronyms:

;; FRDP: Firefox Remote Debugging Protocol
;;       https://firefox-source-docs.mozilla.org/devtools/backend/protocol.html

;;; Code:
(require 'cc-langs)
(require 'comint)
(require 'js)
(require 'json)

(defvar fjrepl--debug nil
  "Non-nil to print debug messages to buffer *fjrepl-debug*.")

(defvar fjrepl--console-actor nil
  "The console actor used to evaluate JavaScript in Firefox.")

(defvar messages-buffer-name) ; Emacs 26.1 byte-compiler.

(defun fjrepl--debug (message)
  "Print MESSAGE, a string, to *fjrep-debug*."
  (when fjrepl--debug
    (let ((messages-buffer-name "*fjrepl-debug*")
          (inhibit-message t))
      (message "%s" (string-trim-left (string-trim-right message))))))

(defun fjrepl--message (format &rest arguments)
  "Print to *Messages* a package herald and FORMAT.
ARGUMENTS will be used for FORMAT, like `messages'."
  (apply #'message (concat "firefox-javascript-repl: " format) arguments))

(defun fjrepl--error (message)
  "Throw an error with a package herald and MESSAGE."
  (error "firefox-javascript-repl: %s" message))

(defvar fjrepl--prompt "FJ> "
  "The prompt used in the Firefox JavaScript buffer.")

(defun fjrepl--input-sender (network string)
  "Send to NETWORK the STRING.  Set `comint-input-sender' to this function."
  (let ((send (substring-no-properties
               (replace-regexp-in-string
                "\n" " " (replace-regexp-in-string
                          "\"" "\\\\\"" string)))))
    (let ((message
           (fjrepl--format-message
            ;; Do not use "eager": true, or stuff like
            ;; "alert('hello')" does not work.
            "{\"type\":\"evaluateJSAsync\",\"text\":\"%s\",\"to\":\"%s\"}"
            send fjrepl--console-actor)))
      (fjrepl--debug message)
      (process-send-string network message))))

(defconst fjrepl--directory
  (file-name-directory load-file-name)
  "The directory in which `firefox-javascript-repl.el' is installed.")

(defun fjrepl--show-quirk ()
  "Show a fun JavaScript quirk in the minibuffer."
  (let ((quirk
         (with-temp-buffer
           (insert-file-contents (expand-file-name
                                  "wtfjs.README.md"
                                  fjrepl--directory))
           (goto-char (point-min))
           (let* ((regexp "^```js\n")
                  (count (count-matches regexp)))
             (re-search-forward regexp nil t (1+ (random count)))
             (let ((start (point)))
               (re-search-forward "^```" nil t)
               (beginning-of-line)
               (backward-char)
               (buffer-substring start (point)))))))
    (let ((minibuffer-message-timeout 7))
      (minibuffer-message
       (with-temp-buffer (js-mode)
                         (insert "// JavaScript quirk of the day:\n")
                         (insert
                          (truncate-string-to-width quirk 100 nil nil "..."))
                         (insert "\n")
                         (insert "// Happy Hacking!")
                         (font-lock-ensure (point-min) (point-max))
                         (buffer-string))))))

(define-derived-mode firefox-javascript-repl--mode comint-mode "FJ"
  "Major mode for interactively evaluating JavaScript expressions in Firefox."
  :syntax-table js-mode-syntax-table
  :interactive nil
  :after-hook
  (progn
    (setq-local comint-prompt-regexp
                (concat "^" (regexp-quote fjrepl--prompt)))
    (setq-local comint-input-sender
                'fjrepl--input-sender)
    (setq-local comint-last-output-start (make-marker))
    (set-marker comint-last-output-start (point-max))
    (setq-local comint-last-input-start (make-marker))
    (set-marker comint-last-input-start (point-max))
    (setq-local comint-last-input-end (make-marker))
    (set-marker comint-last-input-end (point-max))
    (setq-local comint-accum-marker (make-marker))
    (set-marker comint-accum-marker (point-max))
    (setq-local comint-last-prompt (cons (make-marker) (make-marker)))
    (set-marker (car comint-last-prompt) (point-max))
    (when (bobp) (insert fjrepl--prompt))
    (set-marker (process-mark (get-buffer-process (current-buffer)))
                (point))
    (set-marker (cdr comint-last-prompt) (point-max))
    (when (boundp 'comint-indirect-setup-function)
      (setq-local comint-indirect-setup-function 'js-mode))
    (when (fboundp 'comint-indirect-buffer)
      (comint-indirect-buffer))
    (when (fboundp 'comint-fontify-input-mode)
      (comint-fontify-input-mode))
    (make-local-variable 'kill-buffer-hook)
    (when (fboundp 'comint--indirect-cleanup)
      (add-hook 'kill-buffer-hook 'comint--indirect-cleanup))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (let ((network (get-process "firefox-javascript-repl")))
                  (when network (kill-process network)))))
    (fjrepl--show-quirk)))

(defun fjrepl--create-profile-directory ()
  "Create a profile directory."
  (let ((profile-directory (make-temp-file "firefox-javascript-repl-" t)))
    (with-current-buffer
        (find-file-noselect (expand-file-name "user.js" profile-directory))
      (insert "user_pref(\"devtools.debugger.remote-enabled\", true);\n")
      (insert "user_pref(\"devtools.chrome.enabled\", true);\n")
      (insert "user_pref(\"devtools.debugger.prompt-connection\", false);\n")
      ;; Disable Firefox Privacy Policy tab.
      (insert
       "user_pref(\"toolkit.telemetry.reportingpolicy.firstRun\", false);\n")
      ;; Disable "Choose what I share" drop down.
      (insert
       (concat
        "user_pref("
        "\"datareporting.policy.dataSubmissionPolicyBypassNotification\","
        " true);\n"))
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
            (let ((json-object-type 'hash-table))
              (json-read-from-string
               (buffer-substring start (point))))
          (backward-sexp)))))  )

(defun fjrepl--get-result (buffer descend &rest arguments)
  "Return a field from BUFFER.
BUFFER holds the FRDP response messages.  DESCEND is non-nil if
ARGUMENTS represent a decending path through the data structure.
DESCEND is nil if ARGUMENTS should match fields at the current
depth level.  Search from the most recent to the least recent
messages.  Return the filtered value based on the matching
arguments in the hierarchy.  ARGUMENTS is a list of numbers and
strings.  A number is used to pick out an element in a JSON list,
a string is used to pick out an element from a JSON map."
  (with-current-buffer buffer
    (goto-char (point-max))
    (let (result)
      (catch 'match
        (while (setq result (fjrepl--parse-prior-message buffer))
          (catch 'fail
            (dolist (argument arguments) ; check that all arguments match
              (cond ((numberp argument)
                     (setq result (when (vectorp result)
                                    (elt result argument))))
                    ((stringp argument)
                     (if descend
                         (setq result (when (hash-table-p result)
                                        (gethash argument result)))
                       (setq result (and (gethash argument result)
                                         result)))))
              (unless result (throw 'fail nil))))
          (when result (throw 'match result)))))))

(defun fjrepl--remove-results ()
  "Remove the most recent network message from Firefox from the current buffer."
  (with-current-buffer "*firefox-javascript-repl*"
    (save-excursion
      (while (fjrepl--get-result (current-buffer) nil)
        (backward-word)
        (let ((start (point)))
          (forward-word)
          (forward-sexp)
          (delete-region start (point)))))))

(defun fjrepl--extract-result (network response)
  "Filter the process NETWORK.
RESPONSE is a FRDP message received from the browser.  Return the
result of the JavaScript evaluation."
  (fjrepl--debug response)
  (let* ((result (with-temp-buffer
                   (insert response)
                   (fjrepl--get-result
                    (current-buffer) nil "result")))
         (parsed (and result (hash-table-p result)
                      (gethash "result" result)))
         (string (and parsed
                      (format "%s"
                              (if (and parsed (hash-table-p parsed))
                                  (let ((preview (gethash "preview" parsed)))
                                    (if preview
                                        (gethash "items" preview)
                                      (gethash "type" parsed)))
                                (if parsed parsed ""))))))
    (when string
      (comint-output-filter network (concat string "\n" fjrepl--prompt)))))

(defvar fjrepl--count 0 "Progress counter.")

(defun fjrepl--handle-done (name buffer network)
  "Process the startListeners response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the FRDP connection to Firefox."
  (fjrepl--remove-results)
  (set-process-filter network 'fjrepl--extract-result)
  (fjrepl--message
   "Ready for asynchronous JavaScript evaluation %s %s %s %s"
   name buffer network fjrepl--console-actor)
  (with-current-buffer (pop-to-buffer buffer)
    (firefox-javascript-repl--mode)))

(defun fjrepl--stop ()
  "Stop the Firefox process started by `firefox-javascript-repl'."
  (kill-process "firefox-javascript-repl"))

(defun fjrepl--ensure-count (message)
  "If the progress counter is too high, stop Firefox and throw an error.
MESSAGE is the error message."
  (when (> fjrepl--count 59)
    (fjrepl--stop)
    (error message)))

(defun fjrepl--handle-console (name buffer network)
  "Process the startListeners response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the FRDP connection to Firefox."
  (fjrepl--ensure-count "Failed to access Firefox console actor")
  (fjrepl--message "Handling console message %d"
                   (setq fjrepl--count (1+ fjrepl--count)))
  (when (get-buffer buffer)
    (let ((result (fjrepl--get-result buffer nil "startedListeners" "from")))
      (when result
        (let ((console-actor (gethash "from" result)))
          (fjrepl--message "Located console actor %s" console-actor)))
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
the FRDP connection to Firefox."
  (fjrepl--ensure-count "Failed to find Firefox targets")
  (fjrepl--message "Handling target message %d"
                   (setq fjrepl--count (1+ fjrepl--count)))
  (when (get-buffer buffer)
    (let ((console-actor (fjrepl--get-result buffer t "frame" "consoleActor")))
      (when console-actor
        (setq fjrepl--console-actor console-actor)
        (let ((message (fjrepl--format-message
                        (concat
                         "{\"type\":\"startListeners\","
                         "\"to\":\"%s\",\"listeners\":[\"eventListener\"]}")
                        console-actor)))
          (fjrepl--debug message)
          (process-send-string network message)))
      (run-at-time (unless console-actor 1) nil
                   (if console-actor
                       'fjrepl--handle-console 'fjrepl--handle-target)
                   name buffer network))))

(defun fjrepl--handle-tab (name buffer network)
  "Process the listTabs response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the FRDP connection to Firefox."
  (fjrepl--ensure-count "Failed to find Firefox tab")
  (fjrepl--message "Handling tab message %d"
                   (setq fjrepl--count (1+ fjrepl--count)))
  (when (get-buffer buffer)
    (let ((actor (fjrepl--get-result buffer t "tabs" 0 "actor")))
      (when actor
        (let ((message (fjrepl--format-message
                        "{\"type\":\"getTarget\",\"to\":\"%s\"}" actor)))
          (fjrepl--debug message)
          (process-send-string network message)))
      (run-at-time (unless actor 1) nil
                   (if actor 'fjrepl--handle-target 'fjrepl--handle-tab)
                   name buffer network))))

(defun fjrepl--debug-result (network response)
  "Log a message from NETWORK -- RESPONSE -- to *fjrepl-debug*."
  (fjrepl--debug response)
  (internal-default-process-filter network response))

(defun fjrepl--handle-first (name buffer network)
  "Connect to Firefox process and process the first FRDP message from Firefox.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the FRDP connection to Firefox."
  (fjrepl--ensure-count "Failed to detect first message from Firefox")
  (fjrepl--message "Handling first message %d"
                   (setq fjrepl--count (1+ fjrepl--count)))
  (let ((network network))
    (unwind-protect
        (progn (setq network
                     (ignore-errors
                       (open-network-stream name buffer "127.0.0.1" 6000)))
               (when network
                 (set-process-sentinel
                  network
                  (lambda (network event)
                    (fjrepl--message
                     "Network sentinel: %S %S" network
                     (string-trim-left (string-trim-right event)))))))
      (let ((nextp (and network
                        (fjrepl--get-result buffer t "applicationType"))))
        (when nextp
          (set-process-filter network 'fjrepl--debug-result)
          (let ((message "31:{\"to\":\"root\",\"type\":\"listTabs\"}"))
            (fjrepl--debug message)
            (process-send-string network message)))
        (run-at-time (unless nextp 1) nil
                     (if nextp 'fjrepl--handle-tab 'fjrepl--handle-first)
                     name buffer network)))))

;;;###autoload
(defun firefox-javascript-repl ()
  "Run a new instance of Firefox in a new profile.
Set about:config values `devtools.debugger.remote-enabled' to
true, `devtools.chrome.enabled' to true,
`devtools.debugger.prompt-connection' to false, and pass
`-start-debugger-server'.  Firefox will listen on
localhost (127.0.0.1) TCP port 6000."
  (interactive)
  (if (process-status "firefox-javascript-repl")
      (pop-to-buffer "*firefox-javascript-repl*")
    (setq fjrepl--count 0)
    (fjrepl--debug "--MARK--")
    (let* ((profile-directory
            (fjrepl--create-profile-directory))
           (temporary-name (file-name-nondirectory profile-directory))
           (firefox-standard-output-buffer-name
            (concat "*out-" temporary-name "*"))
           (network-name (concat "net-" temporary-name))
           ;; Re-use the network buffer for comint.
           (comint-buffer-name "*firefox-javascript-repl*")
           (firefox-process
            (start-process "firefox-javascript-repl"
                           firefox-standard-output-buffer-name
                           "firefox" ; executable binary name
                           "about:blank"
                           "-profile" profile-directory
                           "-start-debugger-server")))
      (set-process-sentinel firefox-process
                            (lambda (firefox-process event)
                              (fjrepl--message
                               "Firefox standard output sentinel: %S %S"
                               firefox-process
                               (string-trim-left (string-trim-right event)))
                              (when (member
                                     event
                                     '("killed\n"
                                       "finished\n"))
                                (fjrepl--message
                                 "%s %s; deleting %s"
                                 firefox-process
                                 (string-trim-left (string-trim-right event))
                                 profile-directory)
                                (setq fjrepl--console-actor nil)
                                (kill-buffer
                                 firefox-standard-output-buffer-name)
                                (delete-directory profile-directory t))))
      (fjrepl--handle-first network-name comint-buffer-name nil)
      nil)))

(provide 'firefox-javascript-repl)

;;; firefox-javascript-repl.el ends here
