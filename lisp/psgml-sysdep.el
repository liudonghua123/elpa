
(provide 'psgml-sysdep)

(require 'psgml)
(cond
 ((featurep 'xemacs)
  (require 'psgml-lucid))
 (t
  (require 'psgml-other)))
