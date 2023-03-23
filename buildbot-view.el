;; -*- lexical-binding: t; -*-
(require 'buildbot-utils)

(defvar buildbot-view-header-regex "^\\[.*\\]$")

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
   'revision-id (alist-get 'revision revision-info)))

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
   'build build))

(defun buildbot-view-format-step (step)
  (propertize
   (format "\n[%d %s %s]\n"
           (alist-get 'number step)
           (alist-get 'name step)
           (alist-get 'state_string step))
   'step step))

(defun buildbot-view-format-log (log)
  (propertize
   (format "\n[%s]\n"
           (alist-get 'name log))
   'log log))

(provide 'buildbot-view)
