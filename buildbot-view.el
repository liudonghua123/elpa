;; -*- lexical-binding: t; -*-
(require 'buildbot-utils)

(defvar buildbot-view-header-regex "^\\[.*\\]$")
;; 'revision, 'build, 'step, or 'log
(defvar-local buildbot-view-type nil)
(defvar-local buildbot-view-data nil)

(define-derived-mode buildbot-view-mode special-mode "Buildbot view"
  "Buildbot view, a base mode")

(defun buildbot-view-next-header (n)
  (interactive "p")
  (dotimes (_ n)
    (end-of-line 1)
    (re-search-forward buildbot-view-header-regex)
    (beginning-of-line 1)))
(define-key buildbot-view-mode-map "n" 'buildbot-view-next-header)

(defun buildbot-view-previous-header (n)
  (interactive "p")
  (beginning-of-line 1)
  (unless (looking-at buildbot-view-header-regex)
    (re-search-backward buildbot-view-header-regex))
  (dotimes (_ n)
    (re-search-backward buildbot-view-header-regex)))
(define-key buildbot-view-mode-map "p" 'buildbot-view-previous-header)

(defun buildbot-view-format-revision-info (revision-info)
  (propertize
   (format
    "[commit %s]\nAuthor: %s\nDate: %s\n\n%s\n\n%s"
    (alist-get 'revision revision-info)
    (alist-get 'author revision-info)
    (alist-get 'created-at revision-info)
    (alist-get 'comments revision-info)
    (buildbot-view-format-build-stats
     (alist-get 'build-stats revision-info)))
   'revision-id (alist-get 'revision revision-info) 'type 'revision))

(defun buildbot-view-format-build-stats (stats)
  (format "Build stats: Success - %d | Failure - %d | Pending - %d"
          (alist-get 'success stats)
          (alist-get 'failure stats)
          (alist-get 'pending stats)))

(defun buildbot-view-format-build (build)
  (propertize
   (format "\n[%s %s]\n%s"
           (buildbot-get-builder-name-by-id (alist-get 'builderid build))
           (alist-get 'state_string build)
           (string-join
            (mapcar (lambda (test) (alist-get 'test_name test))
                    (alist-get 'failed_tests build))
            "\n"))
   'build build 'type 'build))

(defun buildbot-view-format-step (step)
  (propertize
   (format "\n[%d %s %s]\n"
           (alist-get 'number step)
           (alist-get 'name step)
           (alist-get 'state_string step))
   'step step 'type 'step))

(defun buildbot-view-format-log (log)
  (propertize
   (format "\n[%s]\n"
           (alist-get 'name log))
   'log log 'type 'log))

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

(defun buildbot-build-format (revision-info build steps)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-step steps)
    "\n")))

(defun buildbot-step-format (revision-info build step logs)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-log logs)
    "\n")))

(defun buildbot-log-format (revision-info build step log log-text)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (buildbot-view-format-log log)
   "\n"
   log-text))

(defun buildbot-view-buffer-name (type data)
  (pcase type
    ('revision (format "*buildbot revision %s*"
                       (alist-get 'revision-id data)))
    ('build (format "*buildbot build %d*"
                    (alist-get 'id (alist-get 'build data))))
    ('step (format "*buildbot step %d*"
                   (alist-get 'stepid (alist-get 'step data))))
    ('log (format "*buildbot log %d*"
                   (alist-get 'logid (alist-get 'log data))))))

(defun buildbot-view-open (type data &optional force)
  (let ((buffer-name (buildbot-view-buffer-name type data)))
    (when (or force (not (get-buffer buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (buildbot-view-mode)
        (setq buildbot-view-type type
              buildbot-view-data data)
        (buildbot-view-update)))
    (switch-to-buffer buffer-name)))

(defun buildbot-view-reload ()
  (interactive)
  (buildbot-view-update))
(define-key buildbot-view-mode-map "g" 'buildbot-view-reload)

(defun buildbot-view-revision-open (revision-id)
  (interactive "sRevision (commit hash): ")
  (buildbot-view-load 'revision `((revision-id . ,revision-id))))

(defun buildbot-view-update ()
  (unless (derived-mode-p 'buildbot-view-mode)
    (error "Not in buildbot view mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pcase buildbot-view-type
      ('revision
       (let ((change
              (buildbot-get-change-by-revision
               (alist-get 'revision-id buildbot-view-data))))
         (setf (alist-get 'revision-info buildbot-view-data)
               (buildbot-revision-get-info change))
         (insert (buildbot-revision-format
                  (alist-get 'revision-info buildbot-view-data)
                  (alist-get 'builds change)))))
      ('build
       (insert (buildbot-build-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (buildbot-get-steps-by-buildid
                 (alist-get 'id
                            (alist-get 'build buildbot-view-data))))))
      ('step
       (insert (buildbot-step-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (buildbot-get-logs-by-stepid
                 (alist-get 'stepid
                            (alist-get 'step buildbot-view-data))))))
      ('log
       (insert (buildbot-log-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (alist-get 'log buildbot-view-data)
                (buildbot-api-log-raw
                 (alist-get 'logid
                            (alist-get 'log buildbot-view-data)))))))
    (goto-char (point-min))))

(defun buildbot-view-open-thing-at-point (force)
  (interactive "P")
  (let ((data (copy-tree buildbot-view-data)))
    (pcase (get-text-property (point) 'type)
      ('revision
       (setf (alist-get 'revision-id data)
             (get-text-property (point) 'revision-id))
       (buildbot-view-open 'revision data))
      ('build
       (setf (alist-get 'build data)
             (get-text-property (point) 'build))
       (buildbot-view-open 'build data))
      ('step
       (setf (alist-get 'step data)
             (get-text-property (point) 'step))
       (buildbot-view-open 'step data))
      ('log
       (setf (alist-get 'log data)
             (get-text-property (point) 'log))
       (buildbot-view-open 'log data)))))
(define-key buildbot-view-mode-map (kbd "<return>")
  'buildbot-view-open-thing-at-point)

(provide 'buildbot-view)
