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

;; Compatibility:

;; Tested 2023-05-19 on ppc64le Debian Firefox 102.11.0esr (64-bit).

;;; Code:
(require 'cc-langs)
(require 'js)
(require 'comint)

(defvar fjrepl--console-actor nil
  "The console actor used to evaluate JavaScript in Firefox.")

(defun fjrepl--message (format &rest arguments)
  "Print to *Messages* a package herald and FORMAT.
ARGUMENTS will be used for FORMAT, like `messages'."
  (apply #'message (concat "firefox-javascript-repl: " format) arguments))

(defun fjrepl--error (message)
  "Throw an error with a package herald and MESSAGE."
  (error "firefox-javascript-repl: %s" message))

(defvar fjrepl--prompt "FJ> "
  "The prompt used in the Firefox JavaScript buffer.")

(defun fjrepl--input-sender (process string)
  "Send to PROCESS the STRING.  Set `comint-input-sender' to this function."
  (process-send-string process (fjrepl--format-message
                                ;; Do not use "eager": true, or stuff like
                                ;; "alert('hello')" does not work.
                                (concat "{\"type\":\"evaluateJSAsync\","
                                        "\"text\":\"%s\","
                                        "\"to\":\"%s\"}")
                                (replace-regexp-in-string "\"" "\\\\\"" string)
                                fjrepl--console-actor)))

(define-derived-mode firefox-javascript-repl-mode comint-mode "FJ"
  "Major mode for interactively evaluating JavaScript expressions in Firefox."
  :syntax-table js-mode-syntax-table
  :after-hook
  (progn
    (when (and (string= (buffer-name) "*firefox-javascript-repl*")
               (fjrepl--parse-prior-message (current-buffer))) ; No accidents.
      (delete-region (point-min) (point-max)))
    ;; From js.el.
    ;; Ensure all CC Mode "lang variables" are set to valid values.
    (c-init-language-vars js-mode)
    (setq-local indent-line-function #'js-indent-line)
    (setq-local beginning-of-defun-function #'js-beginning-of-defun)
    (setq-local end-of-defun-function #'js-end-of-defun)
    (setq-local open-paren-in-column-0-is-defun-start nil)
    (setq-local font-lock-defaults
                (list js--font-lock-keywords nil nil nil nil
                      '(font-lock-syntactic-face-function
                        . js-font-lock-syntactic-face-function)))
    (setq-local syntax-propertize-function #'js-syntax-propertize)
    (add-hook 'syntax-propertize-extend-region-functions
              #'syntax-propertize-multiline 'append 'local)
    (add-hook 'syntax-propertize-extend-region-functions
              #'js--syntax-propertize-extend-region 'append 'local)
    (setq-local prettify-symbols-alist js--prettify-symbols-alist)

    (setq-local parse-sexp-ignore-comments t)
    (setq-local which-func-imenu-joiner-function #'js--which-func-joiner)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
    (setq-local comment-end "")
    (setq-local fill-paragraph-function #'js-fill-paragraph)
    (setq-local normal-auto-fill-function #'js-do-auto-fill)

    ;; Parse cache
    (add-hook 'before-change-functions #'js--flush-caches t t)

    ;; Frameworks
    (js--update-quick-match-re)

    ;; Syntax extensions
    (unless (js-jsx--detect-and-enable)
      (add-hook 'after-change-functions #'js-jsx--detect-after-change nil t))
    (js-use-syntactic-mode-name)

    ;; Imenu
    (setq imenu-case-fold-search nil)
    (setq imenu-create-index-function #'js--imenu-create-index)

    ;; for filling, pretend we're cc-mode
    (c-foreign-init-lit-pos-cache)
    (add-hook 'before-change-functions #'c-foreign-truncate-lit-pos-cache nil t)
    (setq-local comment-line-break-function #'c-indent-new-comment-line)
    (setq-local comment-multi-line t)
    (setq-local electric-indent-chars
                (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
    (setq-local electric-layout-rules
                '((?\; . after) (?\{ . after) (?\} . before)))

    (let ((c-buffer-is-cc-mode t))
      ;; FIXME: These are normally set by `c-basic-common-init'.  Should
      ;; we call it instead?  (Bug#6071)
      (make-local-variable 'paragraph-start)
      (make-local-variable 'paragraph-separate)
      (make-local-variable 'paragraph-ignore-fill-prefix)
      (make-local-variable 'adaptive-fill-mode)
      (make-local-variable 'adaptive-fill-regexp)
      ;; While the full CC Mode style system is not yet in use, set the
      ;; pertinent style variables manually.
      (c-initialize-builtin-style)
      (let ((style (cc-choose-style-for-mode 'js-mode c-default-style)))
        (c-set-style style))
      (setq c-block-comment-prefix "* "
            c-comment-prefix-regexp "//+\\|\\**")
      (c-setup-paragraph-variables))

    (goto-char (point-max))

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
    (unless (or (bobp) (bolp)) (insert "\n"))
    (set-marker (car comint-last-prompt) (point-max))
    (insert fjrepl--prompt)
    (set-marker (process-mark (get-buffer-process (current-buffer)))
                (point))
    (set-marker (cdr comint-last-prompt) (point-max))
    (setq-local comint-indirect-setup-function 'js-mode)
    (comint-indirect-buffer)
    (comint-fontify-input-mode)
    (minibuffer-message
     (with-temp-buffer (js-mode)
                       (insert "// JavaScript tip of the day:\n")
                       (font-lock-ensure (point-min) (point-max))
                       (buffer-string)))))

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
            (json-parse-string (buffer-substring start (point)))
          (backward-sexp)))))  )

(defun fjrepl--get-result (buffer descend &rest arguments)
  "Return a field from BUFFER.
BUFFER holds Firefox's Remote Debug Protocol response messages.
DESCEND is non-nil if ARGUMENTS represent a decending path
through the data structure.  DESCEND is nil if ARGUMENTS should
match fields at the current depth level.  Search from the most
recent to the least recent messages.  Return the filtered value
based on the matching arguments in the hierarchy.  ARGUMENTS is a
list of numbers and strings.  A number is used to pick out an
element in a JSON list, a string is used to pick out an element
from a JSON map."
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
                     (if descend
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

(defun fjrepl--extract-result (network response)
  "Filter the process NETWORK.
RESPONSE is a Firefox Debug Protocol message received from the
browser.  Return the result of the JavaScript evaluation."
  (let* ((result (with-temp-buffer
                   (insert response)
                   (fjrepl--get-result
                    (current-buffer) nil "result")))
         (parsed (and (hash-table-p result)
                      (gethash "result" result)))
         (string (and parsed
                      (format "%s" (if (hash-table-p parsed)
                                       (gethash "type" parsed)
                                     parsed)))))
    (when string
      (comint-output-filter network (concat string "\n" fjrepl--prompt)))))

(defun fjrepl--handle-done (name buffer network)
  "Process the startListeners response.
NAME is a string, the process name to use, BUFFER is a string,
the name of the buffer in which to put process output, NETWORK is
the debugger-server connection to Firefox."
  (set-process-filter network 'fjrepl--extract-result)
  (fjrepl--message "Ready for asynchronous JavaScript evaluation %s %s %s"
                   name buffer network)
  (with-current-buffer (pop-to-buffer buffer)
    (firefox-javascript-repl-mode)))

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
        (setq fjrepl--console-actor console-actor)
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
           ;; Re-use the network buffer for comint.
           (comint-buffer-name "*firefox-javascript-repl*")
           (firefox-process
            (start-process "firefox-javascript-repl"
                           process-buffer-name
                           "firefox" ; executable binary name
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
                                (setq fjrepl--console-actor nil)
                                (ignore-errors (kill-buffer comint-buffer-name))
                                (kill-buffer process-buffer-name)
                                (delete-directory profile-directory t))))
      (fjrepl--handle-first network-name comint-buffer-name nil)
      nil)))

(defun firefox-javascript-repl-stop ()
  "Stop the Firefox process started by `firefox-javascript-repl'."
  (interactive)
  (kill-process "firefox-javascript-repl"))

(provide 'firefox-javascript-repl)

;;; firefox-javascript-repl.el ends here
