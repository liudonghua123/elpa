;; -*- lexical-binding: t; -*-
(require 'buildbot-client)

(defvar-local buildbot-revision-revision-id nil)
(defvar buildbot-revision-header-regex "^\\[.*\\]$")

(define-derived-mode buildbot-revision-mode special-mode "Buildbot revision"
  "Buildbot view for a revision")

(define-key buildbot-revision-mode-map "g" 'buildbot-revision-reload)

(defun buildbot-revision-buffer-name (revision)
  (concat "*buildbot revision " revision "*"))

(defun buildbot-revision-load (revision)
  (let ((buffer-name (buildbot-revision-buffer-name revision)))
    (with-current-buffer (get-buffer-create buffer-name)
      (buildbot-revision-mode)
      (setq buildbot-revision-revision-id revision)
      (buildbot-revision-update))
    (switch-to-buffer buffer-name)))

(defun buildbot-revision-update ()
  (unless (derived-mode-p 'buildbot-revision-mode)
    (error "Not in buildbot revision mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (buildbot-revision-format buildbot-revision-revision-id))))

(defun buildbot-revision-open (revision)
  (interactive "sRevision (commit hash): ")
  (buildbot-revision-load revision))

(defun buildbot-revision-reload ()
  (interactive)
  (buildbot-revision-update))

(defun buildbot-revision-format (revision)
  (string-join
   (mapcar 'buildbot-revision-format-build
           (buildbot-get-builds-by-revision revision))
   "\n"))

(defun buildbot-revision-next-header (n)
  (interactive "p")
  (dotimes (_ n)
    (end-of-line 1)
    (re-search-forward buildbot-revision-header-regex)
    (beginning-of-line 1)))
(define-key buildbot-revision-mode-map "n" 'buildbot-revision-next-header)

(defun buildbot-revision-previous-header (n)
  (interactive "p")
  (beginning-of-line 1)
  (unless (looking-at buildbot-revision-header-regex)
    (re-search-backward buildbot-revision-header-regex))
  (dotimes (_ n)
    (re-search-backward buildbot-revision-header-regex)))
(define-key buildbot-revision-mode-map "p" 'buildbot-revision-previous-header)

(defun buildbot-revision-format-build (build)
  (format "[%s %s]\n%s\n"
          (buildbot-get-builder-name-by-id (alist-get 'builderid build))
          (alist-get 'state_string build)
          (string-join
           (mapcar (lambda (test) (alist-get 'test_name test))
                   (alist-get 'failed_tests build))
           "\n")))

(provide 'buildbot-revision)
