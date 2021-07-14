;;; history-capf.el --- Completion-at-point function for comint and eshell history -*- lexical-binding: t; -*-

;; Filename: history-capf.el
;; Description: Completion-at-point function for comint and eshell history
;; Author: jakanakaevangeli <jakanakaevangeli@chiru.no>
;; Created: 2021-07-14
;; Version: 1.0
;; URL: https://github.com/jakanakaevangeli/emacs-capf-autosuggest

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ring)
(eval-when-compile (require 'cl-lib))

(defvar capf-autosuggest-all-completions-only-one)
(defvar capf-autosuggest-active-mode-map)
(defvar comint-input-ring)
(defvar comint-accum-marker)
(defvar comint-use-prompt-regexp)
(defvar eshell-history-ring)
(defvar eshell-last-output-end)
(declare-function capf-autosuggest-mode "capf-autosuggest")
(declare-function eshell-bol "esh-mode")
(declare-function comint-previous-matching-input-from-input "comint")
(declare-function eshell-previous-matching-input-from-input "em-hist")

;;;###autoload
(defun history-capf-comint ()
  "Completion-at-point function for comint input history.
Is only applicable if point is after the last prompt."
  (let ((ring comint-input-ring)
        (beg nil))
    (and ring (ring-p ring) (not (ring-empty-p ring))
         (or (and (setq beg comint-accum-marker)
                  (setq beg (marker-position beg)))
             (and (setq beg (get-buffer-process (current-buffer)))
                  (setq beg (marker-position (process-mark beg)))))
         (>= (point) beg)
         (list beg (if comint-use-prompt-regexp
                       (line-end-position)
                     (field-end))
               (hisotry-capf--completion-table ring)
               :exclusive 'no))))

;;;###autoload
(defun history-capf-eshell ()
  "Completion-at-point function for eshell input history.
Is only applicable if point is after the last prompt."
  (let ((ring eshell-history-ring)
        (beg nil))
    (and ring (ring-p ring) (not (ring-empty-p ring))
         (setq beg eshell-last-output-end)
         (setq beg (marker-position beg))
         (>= (point) beg)
         (list (save-excursion (eshell-bol) (point)) (point-max)
               (hisotry-capf--completion-table ring)
               :exclusive 'no))))

(defun hisotry-capf--completion-table (ring)
  "Return a completion table to complete on RING."
  (let (self)
    (setq
     self
     (lambda (input predicate action)
       (cond
        ((eq action t)
         (cl-loop
          with only-one = capf-autosuggest-all-completions-only-one
          for i below (ring-size ring)
          for elem = (ring-ref ring i)
          if (string-prefix-p input elem)
          if (or (null predicate)
                 (funcall predicate elem))
          if (cl-loop for regex in completion-regexp-list
                      always (string-match-p regex elem))
          if only-one
          return (list elem)
          else collect elem))
        ((eq action nil)
         (complete-with-action
          nil (let ((capf-autosuggest-all-completions-only-one nil))
                (funcall self input predicate t))
          input predicate))
        ((eq action 'lambda)
         (and (ring-member ring input)
              (or (null predicate)
                  (funcall predicate input))
              (cl-loop for regex in completion-regexp-list
                       always (string-match-p regex input))))
        (t (complete-with-action
            action (ring-elements ring) input predicate)))))))

;;;###autoload
(defun history-capf-setup-autosuggest-comint ()
  "Setup capf-autosuggest for history suggestion in comint."
  (capf-autosuggest-mode)
  (add-hook 'capf-autosuggest-capf #'history-capf-comint))

;;;###autoload
(defun history-capf-setup-autosuggest-eshell ()
  "Setup capf-autosuggest for history suggestion in eshell."
  (capf-autosuggest-mode)
  (add-hook 'capf-autosuggest-capf #'history-capf-eshell))

(with-eval-after-load 'capf-autosuggest
  (define-key capf-autosuggest-active-mode-map
    [remap comint-previous-matching-input-from-input]
    #'history-capf-comint-previous-matching-input-from-input)
  (define-key capf-autosuggest-active-mode-map
    [remap eshell-previous-matching-input-from-input]
    #'history-capf-eshell-previous-matching-input-from-input))

(defun history-capf-comint-previous-matching-input-from-input (n)
  "Like `comint-previous-matching-input-from-input'.
But increase arument N by 1, if positive, but not on command
repetition."
  (interactive "p")
  (and (not (memq last-command '(comint-previous-matching-input-from-input
                                 comint-next-matching-input-from-input)))
       (> n 0)
       (setq n (1+ n)))
  (comint-previous-matching-input-from-input n)
  (setq this-command #'comint-previous-matching-input-from-input))

(defun history-capf-eshell-previous-matching-input-from-input (n)
  "Like `eshell-previous-matching-input-from-input'.
But increase arument N by 1, if positive, but not on command
repetition."
  (interactive "p")
  (and (not (memq last-command '(eshell-previous-matching-input-from-input
                                 eshell-next-matching-input-from-input)))
       (> n 0)
       (setq n (1+ n)))
  (eshell-previous-matching-input-from-input n)
  (setq this-command #'eshell-previous-matching-input-from-input))

(provide 'history-capf)
;;; history-capf.el ends here
