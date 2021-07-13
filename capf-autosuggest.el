;;; capf-autosuggest.el --- Mitigate frame creation performance. -*- lexical-binding: t; -*-

;; Filename: capf-autosuggest.el
;; Description: Mitigate frame creation performance.
;; Author: jakanakaevangeli <jakanakaevangeli@chiru.no>
;; Created: 2021-04-23
;; Version: 1.0
;; Keywords: frames
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

(defcustom capf-autosuggest-partial-accept-cmds
  '(forward-word
    forward-char end-of-line move-end-of-line end-of-visual-line
    evil-forward-char evil-end-of-line evil-end-of-visual-line
    evil-end-of-line-or-visual-line evil-middle-of-visual-line
    evil-last-non-blank evil-forward-word-begin evil-forward-word-end
    evil-forward-WORD-begin evil-forward-WORD-end
    capf-autosuggest-accept)
  "Commands that can move point into the auto-suggested overlay."
  :type '(repeat symbol)
  :group 'completion)

(defface capf-autosuggest-face '((t :inherit file-name-shadow))
  "Face used for auto suggestions."
  :group 'completion)

(defvar-local capf-autosuggest-capf t
  "`completion-at-point-functions', used by capf-autossugest.
If t, capf-autosuggest will use `completion-at-point-functions', otherwise it
will use this variable.")

(defvar capf-autosuggest-all-completions-only-one nil
  "Non-nil if only the first result of `all-completions' is of interest.
capf-autosuggest binds this to t around calls to `all-completions'. A dynamic
completion table can take this as a hint to only return a list of one element
for optimization.")

(defvar-local capf-autosuggest--overlay nil)
(defvar-local capf-autosuggest--str "")
(defvar-local capf-autosuggest--region '(nil)
  "Region of `completion-at-point'.")
(defvar-local capf-autosuggest--end-inserted nil
  "End boundary of text inserted `capf-autosuggest--pre-h'.
`capf-autosuggest--post-h' will remove text from point to this boundary if
point is placed before it.")

(defun capf-autosuggest--pre-h ()
  "Insert suggested text if appropriate."
  (and capf-autosuggest-active-mode
       (memq this-command capf-autosuggest-partial-accept-cmds)
       (save-excursion
         (goto-char (overlay-start capf-autosuggest--overlay))
         (insert capf-autosuggest--str)
         (set-marker capf-autosuggest--end-inserted (point)))))

(defun capf-autosuggest--post-h ()
  "Create an auto-suggest overlay.
Remove text inserted by `capf-autosuggest--pre-h', but only from
point forward."
  (let ((end-inserted capf-autosuggest--end-inserted))
    (when-let* ((pos (marker-position end-inserted))
                ((< (point) pos)))
      (delete-region (point) pos))
    (set-marker end-inserted nil))

  (if completion-in-region-mode
      (capf-autosuggest-active-mode -1)
    (when capf-autosuggest-active-mode
      (unless (< (car capf-autosuggest--region) (point)
                 (cdr capf-autosuggest--region))
        (capf-autosuggest-active-mode -1)))

    (unless capf-autosuggest-active-mode
      (pcase (run-hook-wrapped (if (eq capf-autosuggest-capf t)
                                   'completion-at-point-functions
                                 'capf-autosuggest-capf)
                               #'completion--capf-wrapper 'all)
        (`(,_fun ,beg ,end ,table . ,plist)
         (when-let*
             ((completions
               (let ((capf-autosuggest-all-completions-only-one t))
                 ;; Use `all-completions' rather than
                 ;; `completion-all-completions' to bypass completion
                 ;; styles and perform only prefix completions. This makes
                 ;; sense here as we only use the string without the
                 ;; prefix for the overlay.
                 (all-completions (buffer-substring-no-properties beg end)
                                  table (plist-get plist :predicate))))
              (str (substring (car completions) (- end beg)))
              ((/= 0 (length str))))
           (setq capf-autosuggest--region (cons beg end)
                 capf-autosuggest--str (copy-sequence str))
           (move-overlay capf-autosuggest--overlay end end)
           (add-text-properties 0 1 (list 'cursor (length str)) str)
           (add-face-text-property 0 (length str) 'capf-autosuggest-face t str)
           (overlay-put capf-autosuggest--overlay 'after-string str)
           (capf-autosuggest-active-mode)))))))

;;;###autoload
(define-minor-mode capf-autosuggest-mode
  "Auto-suggest first completion at point with an overlay."
  :group 'completion
  (if capf-autosuggest-mode
      (progn
        (setq capf-autosuggest--overlay (make-overlay (point) (point) nil t t))
        (setq capf-autosuggest--end-inserted (make-marker))
        (add-hook 'pre-command-hook #'capf-autosuggest--pre-h nil t)
        (add-hook 'post-command-hook #'capf-autosuggest--post-h nil t))
    (remove-hook 'pre-command-hook #'capf-autosuggest--pre-h t)
    (remove-hook 'post-command-hook #'capf-autosuggest--post-h t)
    (capf-autosuggest-active-mode -1)
    (set-marker capf-autosuggest--end-inserted nil)))

(defvar capf-autosuggest-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap end-of-line] #'capf-autosuggest-accept)
    (define-key map [remap move-end-of-line] #'capf-autosuggest-accept)
    (define-key map [remap end-of-visual-line] #'capf-autosuggest-accept)
    map)
  "Keymap active when an auto-suggestion is shown.")

(defun capf-autosuggest-accept ()
  "Accept current auto-suggestion."
  (interactive)
  (goto-char (cdr capf-autosuggest--region)))

(defun capf-autosuggest--active-acf (beg end _length)
  "Deactivate auto-suggestion on completion region changes."
  ;; `identity' is used to generate slightly faster byte-code
  (when (pcase-let ((`(,beg1 . ,end1) (identity capf-autosuggest--region)))
          (if (< beg beg1)
              (>= end beg1)
            (<= beg end1)))
    (capf-autosuggest-active-mode -1)))

(define-minor-mode capf-autosuggest-active-mode
  "Active when auto-suggested overlay is shown."
  :group 'completion
  (if capf-autosuggest-active-mode
      (add-hook 'after-change-functions #'capf-autosuggest--active-acf nil t)
    (remove-hook 'after-change-functions #'capf-autosuggest--active-acf t)
    (delete-overlay capf-autosuggest--overlay)))

(provide 'capf-autosuggest)
;;; capf-autosuggest.el ends here
