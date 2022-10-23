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
  "A regex for matching paths to a jar file and a file path into the jar file.
Delimited by `!' or `::'")

(defvar-local jarchive--managed-buffer nil
  "This value is t when a buffer is managed by jarchive.")

(defun jarchive--file-name-handler (op &rest args)
  "A `file-name-handler-alist' handler for opening files located in jars.
OP is a `(elisp)Magic File Names' operation and ARGS are any extra argument
provided when calling OP."
  (cond
   ((eq op 'get-file-buffer)
    (let* ((file  (car args))
           (match (string-match jarchive--hybrid-path-regex file))
           (jar (substring file (match-beginning 1) (match-end 1)))
           (file-in-jar (substring file (match-beginning 2))))
      (with-current-buffer (get-buffer-create file)
        (unless (or buffer-read-only jarchive--managed-buffer)
          (message "jarchive: writing buffer %s " args)
          (setq-local jarchive--managed-buffer t)
          (archive-zip-extract jar file-in-jar)
          (setq-local buffer-file-name file)
          (setq-local default-directory (file-name-directory jar))
          (setq-local buffer-offer-save nil)
          (setq buffer-read-only t)
          (set-auto-mode)
          (goto-char 0)
          (set-buffer-modified-p nil))
        (current-buffer))))
   (t (let ((inhibit-file-name-handlers (cons 'jarchive--file-name-handler
                                              (and (eq inhibit-file-name-operation op)
                                                   inhibit-file-name-handlers)))
            (inhibit-file-name-operation op))
        (apply op args)))))


(defun jarchive-setup ()
  (add-to-list 'file-name-handler-alist (cons jarchive--hybrid-path-regex #'jarchive--file-name-handler)))

;; Temporary, for testing
(defmacro comment (&rest body) nil)
(comment
 (jarchive-setup)
 (defvar test-file "/home/user/.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar!/hiccup/page.clj")
 (find-file test-file)
 )

(provide 'jarchive)
