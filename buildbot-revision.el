;; -*- lexical-binding: t; -*-
(require 'buildbot-client)
(require 'buildbot-view)

(defvar-local buildbot-revision-revision-id nil)
(defvar-local buildbot-revision-info nil)

(define-derived-mode buildbot-revision-mode buildbot-view-mode "Buildbot revision"
  "Buildbot view for a revision")

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
(define-key buildbot-revision-mode-map "g" 'buildbot-revision-reload)

(defun buildbot-revision-format (revision-info builds)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-build builds)
    "\n")))

(defun buildbot-revision-get-info (change)
  (list (cons 'revision (alist-get 'revision change))
        (cons 'author (alist-get 'author change))
        (cons 'created-at (buildbot-format-epoch-time
                           (alist-get 'created_at
                                      (alist-get 'sourcestamp change))))
        (cons 'comments (alist-get 'comments change))
        (cons 'build-stats (buildbot-revision-get-build-stats
                            (alist-get 'builds change)))))

(defun buildbot-revision-get-build-stats (builds)
  (let ((results '((success . 0)
                   (failure . 0)
                   (pending . 0)))
        status)
    (seq-do
     (lambda (build)
       (setq status (buildbot-build-status build))
       (setf (alist-get status results)
             (1+ (alist-get status results))))
     builds)
    results))

(defun buildbot-revision-open-build ()
  (interactive)
  (let ((build (get-text-property (point) 'build)))
    (unless build
      (error "Not at a build"))
    (buildbot-build-load build buildbot-revision-info)))
(define-key buildbot-revision-mode-map (kbd "<return>")
  'buildbot-revision-open-build)

(provide 'buildbot-revision)
