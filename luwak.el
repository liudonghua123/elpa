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

(defvar-local luwak-data nil)
(defvar-local luwak-history nil)

(defun luwak-lynx-buffer (url) (format "*luwak-lynx %s*" url))
(defcustom luwak-search-engine "https://html.duckduckgo.com/html?q=%s"
  "Default search engine for use in 'luwak-search'."
  :group 'luwak :type '(string))
(defcustom luwak-url-rewrite-function 'identity
  "Function to rewrite url before loading."
  :group 'luwak :type '(function))

(define-derived-mode luwak-mode special-mode "luwak"
  "Major mode for browsing the web using lynx -dump.")

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
    kmap))

(defun luwak-open (url)
  (interactive "sUrl to open: ")
  (luwak-open-internal url current-prefix-arg 'luwak-add-to-history))

(defun luwak-copy-url ()
  (interactive)
  (when-let ((url (or (get-text-property (point) 'url)
                      (plist-get luwak-data :url))))
    (kill-new url)
    (message "Copied: %s" url)))

(defun luwak-search (query)
  (interactive "sLuwak search query: ")
  (luwak-open (format luwak-search-engine query)))

(defun luwak-open-internal (url no-tor &optional cb)
  (setq url (funcall luwak-url-rewrite-function url))
  (message "Loading %s..." url)
  (set-process-sentinel
   (luwak-start-process-with-torsocks
    current-prefix-arg
    "luwak-lynx" (luwak-lynx-buffer url)
    "lynx" "-dump" "--display_charset" "utf-8" url)
   (lambda (process _)
     (message "Loading %s... Done." url)
     (with-current-buffer (get-buffer-create luwak-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert-buffer-substring (process-buffer process))
         (kill-buffer (process-buffer process))
         (goto-char (point-min))
         (luwak-render-links (luwak-get-links)))
       (unless (derived-mode-p 'luwak-mode) (luwak-mode))
       (if luwak-data
           (plist-put luwak-data :url url)
         (setq luwak-data (list :url url :no-tor no-tor :history-pos 0)))
       (when cb (funcall cb)))
     (display-buffer luwak-buffer))))

(defun luwak-add-to-history ()
  (let ((history-delete-duplicates nil))
    (setq luwak-history (nthcdr (plist-get luwak-data :history-pos)
                                luwak-history))
    (add-to-history 'luwak-history (plist-get luwak-data :url))
    (plist-put luwak-data :history-pos 0)))

(defun luwak-history-backward ()
  (interactive)
  (let ((history-pos
         (1+ (plist-get luwak-data :history-pos))))
    (when (<= (length luwak-history) history-pos)
      (error "Already at the earliest history."))
    (plist-put luwak-data :history-pos history-pos)
    (luwak-open-internal (nth history-pos luwak-history)
                         (plist-get luwak-data :no-tor))))

(defun luwak-history-forward ()
  (interactive)
  (let ((history-pos
         (1- (plist-get luwak-data :history-pos))))
    (when (< history-pos 0)
      (error "Already at the latest history."))
    (plist-put luwak-data :history-pos history-pos)
    (luwak-open-internal (nth history-pos luwak-history)
                         (plist-get luwak-data :no-tor))))

(defun luwak-reload ()
  (interactive)
  (luwak-open-internal
   (plist-get luwak-data :url)
   (plist-get luwak-data :no-tor)))

(defun luwak-follow-link (marker)
  (let ((url (get-text-property marker 'url)))
    (luwak-open-internal
     url (plist-get luwak-data :no-tor) 'luwak-add-to-history)))

(defun luwak-render-links (urls)
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((i 1))
        (dolist (url urls)
          (when (re-search-forward (format "\\[%d\\]" i) nil t)
            (replace-match "")
            (make-text-button (point) (progn (forward-sexp) (point))
                              'url url
                              'help-echo url
                              'action 'luwak-follow-link
                              'face 'button))
          (setq i (1+ i)))))))

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

(provide 'luwak)
