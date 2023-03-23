(defun buildbot-parse-http-header (text)
  (let ((status) (fields))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (re-search-forward "^HTTP.*\\([0-9]\\{3\\}\\).*$")
      (setq status (match-string 1))
      (while (re-search-forward "^\\(.*?\\): \\(.*\\)$" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) fields)))
    (list (cons 'status status) (cons 'fields fields))))

(defun buildbot-delete-http-header ()
  (save-excursion
    (goto-char (point-min))
    (kill-region (point) (progn (re-search-forward "\r?\n\r?\n")
                                (point)))))

(defun buildbot-url-fetch-json (url &optional decompression with-header)
  (with-current-buffer (get-buffer-create buildbot-client-buffer-name)
    (goto-char (point-max))
    (insert "[" (current-time-string) "] Request: " url "\n"))
  (with-current-buffer (url-retrieve-synchronously url t)
    (let ((header) (status) (fields))
      (buildbot-delete-http-header)
      (goto-char (point-min))
      (setq header (buildbot-parse-http-header (car kill-ring))
            status (alist-get 'status header)
            fields (alist-get 'fields header))
      (with-current-buffer buildbot-client-buffer-name
        (insert "[" (current-time-string) "] Response: " status "\n"))
      (when decompression
        (call-process-region (point) (point-max) "gunzip" t t t)
        (goto-char (point-min)))
      (call-interactively 'delete-trailing-whitespace)
      (if (string= status "200")
          (unless (= (point) (point-max))
            (if with-header
                (list
                 (cons 'header fields)
                 (cons 'json (json-read)))
              (json-read)))
        (error "HTTP error: %s" (buffer-substring (point) (point-max)))))))

(defun buildbot-format-attr (attr)
  (string-join (mapcar (lambda (pair)
                           (format "%s=%s" (car pair) (cdr pair)))
                       attr)
               "&"))

(defun buildbot-format-epoch-time (epoch)
  (format-time-string "%Y-%m-%d %a %H:%M:%S %Z" (encode-time
                                                 (decode-time epoch))))


(defun buildbot-build-status (build)
  (let ((state (alist-get 'state_string build)))
    (cond ((equal state "build successful")
           'success)
          ((string-suffix-p "(failure)" state)
           'failure)
          (t 'pending))))

(provide 'buildbot-utils)
