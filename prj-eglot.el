;; project settings for building gpr-query with Alire/editing with eglot -*- no-byte-compile : t -*-

(require 'ada-mode)
;; This require is not needed for the following code, but is needed to
;; ensure ada-mode-hook has sal-ada-mode-setup.

(setq ada-indent-engine 'wisi) ;; ada_language_server 22.0 doesn't support RangeFormatting

(setq ada-xref-tool 'eglot)

(add-hook 'ada-mode-hook #'ada-eglot-setup)

(let* ((gpr-file (expand-file-name "emacs_gpr_query.gpr" (file-name-directory load-file-name)))
       (prj-file (expand-file-name "gpr-query.prj" (file-name-directory load-file-name)))
       (eglot-workspace-configuration (list `(ada (projectFile . ,gpr-file))))

       (project
	(create-alire-project
	 :prj-name "gpr-query main Alire eglot"
	 :prj-file prj-file
	 :gpr-file gpr-file)))

  (wisi-prj-select-cache prj-file nil "Alire.make")

  ;; ada_language_server gets GPR_PROJECT_PATH from its process
  ;; environment, and the gpr file from eglot-workspace-configuration.
  (let ((process-environment
	 (append
	  (copy-sequence process-environment)
	  (wisi-prj-compile-env project)
	  (wisi-prj-file-env project))))

    (eglot 'ada-mode ;; managed-major-mode
	   project ;; project; project-root is server process directory
	   'eglot-lsp-server ;; class
	   'gnat-find-als ;; contact
	   "Ada" ;; language-id
	   ))
  )

;; end of file
