;;; jarchive.el --- Enables navigation into jar archives -*- lexical-binding: t; -*-

;;; Commentary:
;; Jarchive extends Emacs to allow navigation into files contained withing .jar archives.

;;; Code:
(require 'arc-mode)
(require 'project)

(defvar jarchive--hybrid-path-regex
  (rx
   ;; match group 1, the jar file location
   (group "/" (* not-newline) ".jar")
   ;; Potential delimiters between the jar and the file inside the jar
   (or "::" "!")
   ;; match the leading directory delimiter /,
   ;; archvie mode expects none so it's outside match group 2
   (zero-or-one "/")
   ;; match group 2, the file within the archive
   (group (* not-newline) "." (+ alphanumeric))
   line-end)
  "A regex for matching paths to a jar file and a file path into the jar file.
Delimited by `!' or `::'")

(defun jarchive--match-jar (hybrid-filename)
  (string-match jarchive--hybrid-path-regex hybrid-filename)
  (substring hybrid-filename (match-beginning 1) (match-end 1)))

(defun jarchive--match-file (hybrid-filename)
  (string-match jarchive--hybrid-path-regex hybrid-filename)
  (substring hybrid-filename (match-beginning 2) (match-end 2)))

(defmacro jarchive--inhibit (op &rest body)
  "Run BODY with `jarchive--file-name-handler' inhibited for OP."
  `(let ((inhibit-file-name-handlers (cons (quote jarchive--file-name-handler)
                                           (and (eq inhibit-file-name-operation ,op)
                                                inhibit-file-name-handlers)))
         (inhibit-file-name-operation ,op))
     ,@body))

(defvar-local jarchive--jar-path nil)

(defvar-local jarchive--file-in-jar-path nil)

(defvar-local jarchive--visitor-project nil)

(defvar-local jarchive-visiting-project-source-dir "src/"
  "The source directory of the project that is visiting an archived file.
When invoking `jarchive-move-to-visiting-project',
this value is used as the parent directory in the project to save the extracted file.")

(defun jarchive--instruction-message ()
  (message "File opened by jarchive. Type %s to move it into your project."
           (or (car (mapcar 'key-description (where-is-internal 'jarchive-move-to-visiting-project)))
               "M-x jarchive-move-to-visiting-project")))

(defun jarchive--file-name-handler (op &rest args)
  "A `file-name-handler-alist' handler for opening files located in jars.
OP is a `(elisp)Magic File Names' operation and ARGS are any extra argument
provided when calling OP."
  (cond
   ((eq op 'get-file-buffer)
    (let* ((file  (car args))
           (jar (jarchive--match-jar file))
           (file-in-jar  (jarchive--match-file file))
           ;; Use a different filename that doesn't match `jarchive--hybrid-path-regex'
           ;; so that this handler will not deal with existing open buffers.
           (buffer-file (concat jar ":" file-in-jar))
           (visitor-project (project-current)))
      (or (find-buffer-visiting buffer-file)
          (with-current-buffer (create-file-buffer buffer-file)
            (archive-zip-extract jar file-in-jar)
            (goto-char 0)
            (setq-local buffer-file-name buffer-file)
            (setq-local buffer-offer-save nil)
            (setq-local buffer-read-only t)
            (set-auto-mode)
            (jarchive--managed-mode 1)
            (when visitor-project ;; Allow the user to move to the visitor project later.
              (setq-local jarchive--jar-path jar)
              (setq-local jarchive--file-in-jar-path file-in-jar)
              (setq-local jarchive--visitor-project visitor-project))
            (set-buffer-modified-p nil)
            (jarchive--instruction-message)
            (current-buffer)))))
   (t (jarchive--inhibit op (apply op args)))))

(defvar jarchive-mode-map (make-sparse-keymap)
  "A keymap that is active in buffers opened by jarchive.")

;;;###autoload
(define-minor-mode jarchive--managed-mode
  "Mode for buffers opened by jarchive mode."
  :init-value nil
  :ligher nil
  :interactive nil
  :keymap jarchive-mode-map)

(defun jarchive--visting-project-location ()
  (concat (project-root jarchive--visitor-project)
          jarchive-visiting-project-source-dir
          jarchive--file-in-jar-path))

(defun jarchive-move-to-visiting-project ()
  "Move the currently archived file into the project that visited it.
The file will be moved under the `jarchive-visiting-project-source-dir' and saved."
  (interactive)
  (cond ((not jarchive--managed-mode)
         (message "This command is only available in jarchive--managed-mode"))
        ((not jarchive--visitor-project)
         (message "This buffer was not visited from a known project. Nothing to do."))
        (t
         (let* ((new-location (jarchive--visting-project-location)))
           (write-file new-location t)
           (revert-buffer t t)
           (setq-local buffer-read-only nil)
           (jarchive--managed-mode -1)
           (message "Moved to %s" new-location)))))

;;;###autoload
(defun jarchive-setup ()
  (interactive)
  (add-to-list 'file-name-handler-alist (cons jarchive--hybrid-path-regex #'jarchive--file-name-handler)))

;; Temporary, for testing
(defmacro comment (&rest body) nil)
(comment
 (jarchive-setup)
 (defvar test-file "/home/user/.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar!/hiccup/page.clj")
 (find-file test-file)
 )

(provide 'jarchive)
