;;; buildbot-utils.el --- Commonly used utilities. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.
;; 
;; This file is part of buildbot.el.
;; 
;; buildbot.el is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; buildbot.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with buildbot.el.  If not, see <https://www.gnu.org/licenses/>.

(defvar buildbot-client-buffer-name "*buildbot api*")

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

(defun buildbot-url-fetch-raw (url &optional decompression with-header)
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
                 (cons 'json (buffer-string)))
              (buffer-string)))
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

(defun buildbot-step-guess-status (step)
  (let ((state (alist-get 'state_string step)))
    (cond ((string-suffix-p "(warnings)" state)
           'pending)
          ((string-suffix-p "(failure)" state)
           'failure)
          ((string-suffix-p "done" state)
           'success)
          ((string-suffix-p "ing" state)
           'pending)
          ((string-suffix-p "finished" state)
           'success)
          (t 'success))))

(defun buildbot-status-face (status)
  (pcase status
    ('success 'success)
    ('failure 'error)
    (_ 'warning)))

(defun buildbot-get-build-stats (builds)
  (let ((results (copy-tree '((success . 0)
                              (failure . 0)
                              (pending . 0))))
        (status))
    (seq-do
     (lambda (build)
       (setq status (buildbot-build-status build))
       (setf (alist-get status results)
             (1+ (alist-get status results))))
     builds)
    results))

(defun buildbot-get-revision-info-from-change (change)
  (list
   (assq 'revision change)
   (assq 'author change)
   (cons 'created-at
         (buildbot-format-epoch-time
          (alist-get 'when_timestamp change)))
   (assq 'comments change)))

(defun buildbot-get-revision-and-changes-info (changes)
  "Get revision-info and builds from a set of changes of the same revision."
  (let* ((first-change (elt changes 0))
         (revision-info (buildbot-get-revision-info-from-change first-change))
         (changes-info
          (mapcar (lambda (change)
                    (list
                     (assq 'branch change)
                     (assq 'builds change)
                     (cons 'build-stats
                           (buildbot-get-build-stats
                            (alist-get 'builds change)))
                     (assq 'revision first-change)))
                  changes)))
    `((revision-info . ,revision-info) (changes-info . ,changes-info))))

(provide 'buildbot-utils)
