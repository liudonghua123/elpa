
;; Listing projects

(defun javaimp-list-projects (buffer)
  (unless (bufferp buffer)
    (setq buffer (get-buffer-create "*Javaimp Projects*")))
  (with-current-buffer buffer
    (javaimp-list-mode)
    (javaimp-list-projects--refresh)
    (tabulated-list-print))
  (display-buffer buffer)
  nil)

(defvar javaimp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'javaimp-list-forget-project)
    (define-key map "m" 'javaimp-list-display-project)
    (define-key map "i" 'javaimp-list-display-project)
    map))

(define-derived-mode javaimp-list-mode tabulated-list-mode "Javaimp Project List"
  "Major mode for listing the projects known by Javaimp."
  (setq tabulated-list-format [("Artifact" 20 #'javaimp-list--artifact-lessp)
                               ("Group" 20 #'javaimp-list-group-lessp)
                               ("Version" 7 #'nil)
                               ("Build file" (length "build.gradle.kts") nil)
                               ("Loaded at" 10 t)
                               ("Directory" 0 nil)
                               ])
  ;; (setq tabulated-list-sort-key (cons "Artifact" nil))
  ;; tabulated-list-entries if function; tabulated-list-printer; tabulated-list-padding
  (setq buffer-undo-list t)
  (add-hook 'tabulated-list-revert-hook #'javaimp-list--refresh nil t))

(defun javaimp-list--artifact-lessp (entry1 entry2)
  ;; TODO sort only by roots
  nil
  )
(defun javaimp-list--group-lessp (entry1 entry2)
  ;; TODO sort only by roots
  nil
  )

(defun javaimp-list--refresh ()
  "Recompute the list of projects for the Javaimp Project List
buffer."
  (setq tabulated-list-entries nil)

  (dolist (node (javaimp--collect-nodes-from-tree javaimp-project-forest))
    (let* ((m (javaimp-node-contents node))
           (pm (and (javaimp-node-parent node)
                    (javaimp-node-contents (javaimp-node-parent node))))
           (mid (javaimp-module-id m))
           (pmid (and pm (javaimp-module-id pm)))
           (root (javaimp--get-root node))

           ;; TODO use digits, as in proced
           (depth (let ((ptr node)
                        (res 0))
                    (while (setq ptr (javaimp-node-parent ptr))
                      (setq res (1+ res)))
                    res))
           (artifact (concat (make-string depth ? ) (javaimp-id-artifact mid)))

           ;; show group and version only if different from parent
           (group (if (and pmid
                           (javaimp-id-group mid)
                           (javaimp-id-group pmid)
                           (equal (javaimp-id-group mid) (javaimp-id-group pmid)))
                      "*"
                    (javaimp-id-group mid)))
           (version (if (and pmid
                             (javaimp-id-version mid)
                             (javaimp-id-version pmid)
                             (equal (javaimp-id-version mid) (javaimp-id-version pmid)))
                        "*"
                      (javaimp-id-version mid)))

           ;; buttonize filename only if file exists
           (basename (file-name-nondirectory (javaimp-module-file m)))
           (file (if (file-exists-p (javaimp-module-file m))
                     `(,basename
                       face link
                       follow-link t
                       help-echo "Visit file"
                       action ,(lambda (_btn)
                                 (find-file (javaimp-module-file m))))
                   (propertize basename 'face 'warning)))

           (time-format
            (let ((daystart-dec (decode-time)))
              (setf (decoded-time-second daystart-dec) 0)
              (setf (decoded-time-minute daystart-dec) 0)
              (setf (decoded-time-hour daystart-dec) 0)
              (if (time-less-p (javaimp-module-load-ts m)
                               (encode-time daystart-dec))
                  "%c" "%X")))
           (load-ts time-format (javaimp-module-load-ts m))

           (absdir (file-name-directory (javaimp-module-file m)))
           ;; show dir relatively to root if have parent
           (dir-name
            (if pm
                (let ((root-dir (file-name-directory
                                 (javaimp-module-file
                                  (javaimp-node-contents root)))))
                  (file-relative-name dir root-dir))
              absdir))
           (dir `(,dir-name
                  face link
                  follow-link t
                  help-echo "Visit directory"
                  action ,(lambda (_btn)
                            (find-file absdir))))
           ))
    (push (list mid (vector
                     artifact (or group "--") (or version "--")
                     dir file load-ts))
          tabulated-list-entries))
  ;; TODO sort by group / artifact
  (tabulated-list-init-header))

;; TODO option to forget everything loaded from the same file
(defun javaimp-list--forget-project ()
  "Forget project tree containing project at point."
  (interactive)

  ;; TODO go to top-level
  (setq javaimp-project-forest
	(seq-remove (lambda (node)
		      (equal (javaimp-module-file-orig (javaimp-node-contents node))
			     build-file))
		    javaimp-project-forest))

  (let ((pos (point)))
    (delete-process (tabulated-list-get-id))
    (revert-buffer)
    (goto-char (min pos (point-max)))
    (if (eobp)
        (forward-line -1)
      (beginning-of-line))))

(defun javaimp-list--display-project ()
  "Display detailed information for project at point."
  ;;
  ;; TODO detailed, with-output-to-temp-buffer

  )
