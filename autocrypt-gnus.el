;;; autocrypt-gnus.el --- Autocrypt for Gnus -*- lexical-binding:t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>

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
(require 'gnus-art)
(require 'mm-decode)
(require 'autocrypt)

;;;###autoload
(defun autocrypt-gnus--install ()
  "Prepare autocrypt for Gnus."
  (add-hook 'gnus-select-article-hook #'autocrypt-process-header nil t))

(defun autocrypt-gnus--uninstall ()
  "Undo `autocrypt-gnus--install'."
  (remove-hook 'gnus-article-prepare-hook #'autocrypt-process-header t))

(defun autocrypt-gnus--get-header (header)
  "Return value for HEADER from current message."
  (gnus-fetch-original-field header))

(defun autocrypt-gnus--get-part (index)
  "Return the INDEX'th part of the message as a string."
  (save-window-excursion
    (let ((content nil))
      (condition-case nil
          (gnus-article-part-wrapper
           (1+ index)
           (lambda (&optional handle _arg _event)
             (unless handle
               (gnus-article-jump-to-part (1+ index))
               (setq handle (get-text-property (point) 'gnus-data)))
             (with-temp-buffer
               (mm-insert-part handle)
               (setq content (buffer-string)))))
        (error))
      content)))

(provide 'autocrypt-gnus)

;;; autocrypt-gnus.el ends here
