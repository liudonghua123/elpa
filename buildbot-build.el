(require 'buildbot-client)
(define-derived-mode buildbot-build-mode special-mode "Buildbot build"
  "Buildbot view for a build")

(defvar-local buildbot-build-build-id nil)
(defvar-local buildbot-build-info nil)
(defun buildbot-build-buffer-name (buildid)
  (concat "*buildbot build " (number-to-string buildid) "*"))

(defun buildbot-build-load (buildid)
  (let ((buffer-name (buildbot-build-buffer-name buildid)))
    (with-current-buffer (get-buffer-create buffer-name)
      (buildbot-build-mode)
      (setq buildbot-build-build-id buildid)
      (buildbot-build-update))
    (switch-to-buffer buffer-name)))

(defun buildbot-build-update ()
  (unless (derived-mode-p 'buildbot-build-mode)
    (error "Not in buildbot build mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((steps (buildbot-get-steps-by-buildid buildbot-build-build-id)))
      (insert (buildbot-build-format steps))
      (goto-char (point-min)))))

(defun buildbot-build-open (buildid)
  (interactive "sBuildi ID: ")
  (buildbot-build-load (string-to-number buildid)))

(defun buildbot-build-format (steps)
  (string-join
   (mapcar 'buildbot-build-format-step steps)
   "\n"))

(defun buildbot-build-format-step (step)
  (propertize
   (format "\n[%d %s %s]\n"
           (alist-get 'number step)
           (alist-get 'name step)
           (alist-get 'state_string step))))

(provide 'buildbot-build)
