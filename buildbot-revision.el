;; -*- lexical-binding: t; -*-
(require 'buildbot-client)

(defvar-local buildbot-revision-revision-id nil)
(defvar-local buildbot-revision-info nil)
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
    (let ((change
           (buildbot-get-change-by-revision buildbot-revision-revision-id)))
      (setq buildbot-revision-info
            (buildbot-revision-get-info change))
      (insert (buildbot-revision-format buildbot-revision-info
                                        (alist-get 'builds change)))
      (goto-char (point-min)))))

(defun buildbot-revision-open (revision)
  (interactive "sRevision (commit hash): ")
  (buildbot-revision-load revision))

(defun buildbot-revision-reload ()
  (interactive)
  (buildbot-revision-update))

(defun buildbot-revision-format (revision-info builds)
  (concat
   (buildbot-revision-format-info revision-info)
   "\n"
   (string-join
    (mapcar 'buildbot-revision-format-build builds)
    "\n")))

(defun buildbot-revision-get-info (change)
  (list (cons 'revision (alist-get 'revision change))
        (cons 'author (alist-get 'author change))
        (cons 'created-at (buildbot-format-epoch-time
                     (alist-get 'created_at
                                (alist-get 'sourcestamp change))))
        (cons 'comments (alist-get 'comments change))))

(defun buildbot-revision-format-info (info)
  (format
   "commit %s\nAuthor: %s\nDate: %s\n\n%s"
   (alist-get 'revision info)
   (alist-get 'author info)
   (alist-get 'created-at info)
   (alist-get 'comments info)))

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
  (propertize
   (format "\n[%s %s]\n%s"
           (buildbot-get-builder-name-by-id (alist-get 'builderid build))
           (alist-get 'state_string build)
           (string-join
            (mapcar (lambda (test) (alist-get 'test_name test))
                    (alist-get 'failed_tests build))
            "\n"))
   'buildid (alist-get 'id build)))

(provide 'buildbot-revision)
