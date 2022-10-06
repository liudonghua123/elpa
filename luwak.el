;; -*- lexical-binding: t; -*-
(defvar luwak-buffer "*luwak*")
(defun luwak-lynx-buffer (url) (format "*luwak-lynx %s*" url))

(defun luwak-open (url)
  (interactive "sUrl to open: ")
  (setq url (eww--dwim-expand-url url))
  (set-process-sentinel
   (start-process-with-torsocks
    current-prefix-arg
    "luwak-lynx" (luwak-lynx-buffer url)
    "lynx" "-dump" "--display_charset" "utf-8" url)
   (lambda (process _)
     (with-current-buffer (get-buffer-create luwak-buffer)
       (erase-buffer)
       (insert-buffer-substring (process-buffer process))
       (kill-buffer (process-buffer process))
       (goto-char (point-min))
       (luwak-render-links (luwak-get-links)))
     (display-buffer luwak-buffer))))

(defun luwak-follow-link (marker)
  (luwak-open
   (get-text-property marker 'url)))

(defun luwak-render-links (urls)
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((i 1))
        (dolist (url urls)
          (when (re-search-forward (format "\\[%d\\]" i) nil t)
            (replace-match "")
            (make-text-button (point) (1+ (point))
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
