;;; jarchive.el --- Enables navigation into jar archives -*- lexical-binding: t; -*-

;;; Commentary:
;; Jarchive extends Emacs to allow navigation into files contained withing .jar archives.

;;; Code:
(require 'arc-mode)

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
  "A regex for matching jar files to be opened by eglot and clojure-lsp")

(defvar jarchive--file->buffer
  (make-hash-table :test 'equal)
  "A hash table mapping a file name to a buffer.")

;; Maybe I can use find-buffer-visiting, but right now it creates an infinite loop
(defun jarchive--find-buffer-visiting (file)
  (when-let ((existing-buffer (gethash file jarchive--file->buffer)))
    (if (buffer-live-p existing-buffer)
        existing-buffer
      (remhash file jarchive--file->buffer)
      nil)))

(defun jarchive--file-name-handler (op &rest args)
  "A `file-name-handler-alist' handler for opening files located in jars.
OP is a `(elisp)Magic File Names' operation and ARGS are any extra argument
provided when calling OP."
  (cond
   ((eq op 'get-file-buffer)
    (let* ((file  (car args))
           (match (string-match jarchive--hybrid-path-regex file))
           (jar   (substring file (match-beginning 1) (match-end 1)))
           (source-file (substring file (match-beginning 2))))
      (if-let ((existing-buffer (jarchive--find-buffer-visiting file)))
          existing-buffer
        ;; Can I do this with generate-new-buffer
        (with-current-buffer (create-file-buffer file)
          (message "Unzipping %s to show %s" jar source-file)
          (archive-zip-extract jar source-file)
          (puthash file (current-buffer) jarchive--file->buffer)
          (setq-local buffer-file-name file)
          (setq-local default-directory (file-name-directory jar))
          (setq-local buffer-read-only t)
          (setq-local buffer-offer-save nil)
          (set-auto-mode)
          (set-buffer-modified-p nil)
          (goto-char 0)
          (current-buffer)))))
   (t (let ((inhibit-file-name-handlers (cons 'jarchive--file-name-handler
                                              (and (eq inhibit-file-name-operation op)
                                                   inhibit-file-name-handlers)))
            (inhibit-file-name-operation op))
        (apply op args)))))

(defun jarchive--kill-buffer (file)
  (kill-buffer (jarchive--find-buffer-visiting file))
  (remhash file jarchive--file->buffer))

;;(find-file "/home/user/.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar!hiccup/page.clj")
;;(jarchive--kill-buffer "/home/user/.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar!hiccup/page.clj")

(defun jarchive-setup ()
  (add-to-list 'file-name-handler-alist (cons jarchive--hybrid-path-regex #'jarchive--file-name-handler)))

(provide 'jarchive)
