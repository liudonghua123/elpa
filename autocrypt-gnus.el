;;; autocrypt-gnus.el --- Autocrypt for Gnus -*- lexical-binding:t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.4.0
;; Keywords: comm
;; Package-Requires: ((emacs "25.1"))
;; URL: https://git.sr.ht/~zge/autocrypt

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:

;; MUA specific functions for Gnus
;;
;; Setup with (add-hook 'gnus-load-hook #'autocrypt-mode)

;;; Code:

(require 'gnus)

;;;###autoload
(cl-defmethod autocrypt-load-system ((_mode (derived-mode gnus-mode)))
  "Load this module."
  (require 'autocrypt-gnus))

(cl-defmethod autocrypt-mode-hooks ((_mode (derived-mode gnus-mode)))
  "Return the hook to install autocrypt."
  '(gnus-article-prepare-hook))

(cl-defmethod autocrypt-get-header ((_mode (derived-mode gnus-mode))
                                    header)
  "Return the value for HEADER."
  (gnus-fetch-original-field header))

(provide 'autocrypt-gnus)

;;; autocrypt-gnus.el ends here
