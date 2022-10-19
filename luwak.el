;; -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Yuchen Pei.
;; 
;; This file is part of luwak.
;; 
;; luwak is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; luwak is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with luwak.  If not, see <https://www.gnu.org/licenses/>.

(defvar luwak-buffer "*luwak*")

(defvar-local luwak-data (:url nil :dump nil :history-pos nil :no-tor nil))
(defvar-local luwak-history nil)

(defun luwak-lynx-buffer (url) (format "*luwak-lynx %s*" url))
(defcustom luwak-search-engine "https://html.duckduckgo.com/html?q=%s"
  "Default search engine for use in 'luwak-search'."
  :group 'luwak :type '(string))
(defcustom luwak-url-rewrite-function 'identity
  "Function to rewrite url before loading."
  :group 'luwak :type '(function))
(defcustom luwak-tor-switch nil
  "Switch behaviour of prefix arg concerning the use of tor.

When nil, use tor by default, and not use it with a prefix arg.  
When non-nill, swap the tor-switch in prefix-arg effect."
  :group 'luwak :type '(boolean))
(defcustom luwak-max-history-length 100
  "Maximum history length."
  :group 'luwak :type '(natnum))
(defcustom luwak-render-link-function 'luwak-render-link-id
  "Function to render a link."
  :group 'luwak :type '(choice (const luwak-render-link-id)
                               (const luwak-render-link-forward-sexp)))

(put luwak-history 'history-length luwak-max-history-length)

(defun luwak-toggle-tor-switch ()
  (interactive)
  (setq luwak-tor-switch (not luwak-tor-switch)))

(defun luwak-mode-name ()
  (concat "luwak "
          (cond
           ((null luwak-data) "Tor unknown")
           ((plist-get luwak-data :no-tor) "Tor off")
           (t "Tor on"))))

(defvar luwak-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\t" 'forward-button)
    (define-key kmap [backtab] 'backward-button)
    (define-key kmap "g" 'luwak-reload)
    (define-key kmap "l" 'luwak-history-backward)
    (define-key kmap "r" 'luwak-history-forward)
    (define-key kmap "w" 'luwak-copy-url)
    (define-key kmap "o" 'luwak-open)
    (define-key kmap "s" 'luwak-search)
    (define-key kmap "d" 'luwak-save-dump)
    kmap))

(define-derived-mode luwak-mode special-mode (luwak-mode-name)
  "Major mode for browsing the web using lynx -dump.")

(defun luwak-open (url)
  "Open URL in luwak."
  (interactive "sUrl to open: ")
  (luwak-open-url
   url (xor luwak-tor-switch current-prefix-arg) 'luwak-add-to-history))

(defun luwak-copy-url ()
  (interactive)
  (when-let ((url (or (get-text-property (point) 'url)
                      (plist-get luwak-data :url))))
    (kill-new url)
    (message "Copied: %s" url)))

(defun luwak-search (query)
  "Search QUERY using 'luwak-search-engine'."
  (interactive "sLuwak search query: ")
  (luwak-open (format luwak-search-engine query)))

(defun luwak-open-url (url no-tor &optional cb)
  (setq url (funcall luwak-url-rewrite-function url))
  (message "Loading %s..." url)
  (set-process-sentinel
   (luwak-start-process-with-torsocks
    no-tor
    "luwak-lynx" (luwak-lynx-buffer url)
    "lynx" "-dump" "--display_charset" "utf-8" url)
   (lambda (process _)
     (message "Loading %s... Done." url)
     (with-current-buffer (get-buffer-create luwak-buffer)
       (luwak-open-internal
        url
        (with-current-buffer (process-buffer process) (buffer-string))
        (or (plist-get luwak-data :history-pos) 0)
        no-tor)
       (kill-buffer (process-buffer process))
       (when cb (funcall cb))
       (goto-char (point-min)))
     (display-buffer luwak-buffer))))

(defun luwak-open-internal (url dump history-pos no-tor)
  (with-current-buffer (get-buffer-create luwak-buffer)
    (unless (derived-mode-p 'luwak-mode) (luwak-mode))
    (setq luwak-data (list :url url :no-tor no-tor
                           :history-pos history-pos :dump dump))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert dump)
      (luwak-render-links (luwak-get-links)))
    (setq mode-name (luwak-mode-name))
    (goto-char (point-min))))

(defun luwak-add-to-history ()
  (let ((history-delete-duplicates nil))
    (setq luwak-history (nthcdr (plist-get luwak-data :history-pos)
                                luwak-history))
    (add-to-history 'luwak-history
                    (cons (plist-get luwak-data :url)
                          (plist-get luwak-data :dump)))
    (plist-put luwak-data :history-pos 0)))

(defun luwak-history-backward ()
  (interactive)
  (let ((history-pos
         (1+ (plist-get luwak-data :history-pos))))
    (when (<= (length luwak-history) history-pos)
      (error "Already at the earliest history."))
    (luwak-history-open history-pos)))

(defun luwak-history-forward ()
  (interactive)
  (let ((history-pos
         (1- (plist-get luwak-data :history-pos))))
    (when (< history-pos 0)
      (error "Already at the latest history."))
    (luwak-history-open history-pos)))

(defun luwak-history-open (history-pos)
  (let ((pair (nth history-pos luwak-history))
        (len (length luwak-history)))
      (luwak-open-internal (car pair) (cdr pair) history-pos
                           (plist-get luwak-data :no-tor))
      (message "Loaded history %d/%d: %s"
               (- len history-pos) len (car pair))))

(defun luwak-reload ()
  (interactive)
  (luwak-open-url
   (plist-get luwak-data :url)
   (plist-get luwak-data :no-tor)))

(defun luwak-follow-link (marker)
  (let ((url (get-text-property marker 'url)))
    (luwak-open-url
     url (plist-get luwak-data :no-tor) 'luwak-add-to-history)))

(defun luwak-render-links (urls)
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((i 1))
        (dolist (url urls)
          (luwak-render-link-id i url)
          (setq i (1+ i)))))))

(defun luwak-render-link-forward-sexp (idx url)
  "Render a link using forward-sexp."
  (when (re-search-forward (format "\\[%d\\]" idx) nil t)
    (replace-match "")
    (make-text-button (point) (progn (forward-sexp) (point))
                      'url url
                      'help-echo url
                      'action 'luwak-follow-link
                      'face 'button)))

(defun luwak-render-link-id (idx url)
  "Render a link by its id."
  (when (re-search-forward (format "\\[%d\\]" idx) nil t)
    (make-text-button (match-beginning 0) (match-end 0)
                      'url url
                      'help-echo url
                      'action 'luwak-follow-link
                      'face 'button)))

(defun luwak-get-links ()
  "Get links and remove the reference section if any."
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^References\n\n\\(\\ *Visible links:\n\\)?" nil t)
        (let ((ref-beg (match-beginning 0))
              (results))
          (while (re-search-forward "^\\ *\\([0-9]+\\)\\.\\ *\\(.*\\)$" nil t)
            (push (match-string 2) results))
          (delete-region ref-beg (point-max))
          (reverse results))))))

(defun luwak-start-process-with-torsocks (no-tor name buffer program &rest program-args)
  (if no-tor
      (apply 'start-process (append (list name buffer program) program-args))
    (apply 'start-process
           (append (list name buffer "torsocks" program) program-args))))

(defun luwak-save-dump (file-name)
  (interactive
   (list
    (read-file-name (format "Write dump of %s to: " (plist-get luwak-data :url))
                    default-directory)))
  (let ((dump (plist-get luwak-data :dump)))
    (with-temp-buffer
      (insert dump)
      (write-file file-name)))
  (message "Wrote %s." file-name))

(defun luwak-make-filename (name &optional sep)
  "Convert name to filename by replacing special chars with sep."
  (unless sep (setq sep "-"))
  (replace-regexp-in-string "[[:punct:][:space:]\n\r]+" sep
                            (string-trim name)))

(provide 'luwak)
