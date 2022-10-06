;; -*- lexical-binding: t; -*-
(defvar luwak-buffer "*luwak*")

(defvar-local luwak-data nil)
(defvar-local luwak-history nil)

(defun luwak-lynx-buffer (url) (format "*luwak-lynx %s*" url))

(define-derived-mode luwak-mode special-mode "luwak"
  "Major mode for browsing the web using lynx -dump.")

(defvar luwak-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\t" 'forward-button)
    (define-key kmap [backtab] 'backward-button)
    (define-key kmap "g" 'luwak-reload)
    (define-key kmap "l" 'luwak-history-backward)
    (define-key kmap "r" 'luwak-history-forward)
    kmap))

(defun luwak-open (url)
  (interactive "sUrl to open: ")
  (setq url (eww--dwim-expand-url url))
  (luwak-open-internal url current-prefix-arg 'luwak-add-to-history))

(defun luwak-open-internal (url no-tor &optional cb)
  (set-process-sentinel
   (start-process-with-torsocks
    current-prefix-arg
    "luwak-lynx" (luwak-lynx-buffer url)
    "lynx" "-dump" "--display_charset" "utf-8" url)
   (lambda (process _)
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
                              'action 'luwak-follow-link
                              'face 'button))
          (setq i (1+ i)))))))

(defun luwak-get-links ()
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^References\n\n\\(\\ *Visible links:\n\\)?")
      (let ((results))
        (while (re-search-forward "^\\ *\\([0-9]+\\)\\.\\ *\\(.*\\)$" nil t)
          (push (match-string 2) results))
        (reverse results)))))

(provide 'luwak)
