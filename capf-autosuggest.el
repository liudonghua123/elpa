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
    forward-char end-of-line end-of-visual-line
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
capf-autosuggest binds this to t around calls to `all-completions'. Dynamic
completion table can take this as a hint to only return a list of one element
for optimization.")

(defvar-local capf-autosuggest--overlay nil)
(defvar-local capf-autosuggest--str "")
(defvar-local capf-autosuggest--end-boundary nil
  "End boundary of text inserted `capf-autosuggest--pre-h'.
`capf-autosuggest--post-h' will remove text from point to this boundary if
point is placed before it.")

(defun capf-autosuggest--pre-h ()
  "Remove autosuggest overlay.
If `this-command' is a member of
`capf-autosuggest-partial-accept-cmds', replace it with actual
text so that the command that is about to be executed, can move
point into the text. `capf-autosuggest--post-h' will arrange for
the appropriate remaining text to be removed and replaced with an
overlay again."
  ;; TODO: `buffer-modified-tick'
  (when-let* (((memq this-command capf-autosuggest-partial-accept-cmds))
              (overlay capf-autosuggest--overlay)
              ((overlay-buffer capf-autosuggest--overlay)))
    (save-excursion
      (goto-char (overlay-start overlay))
      (insert capf-autosuggest--str)
      (if capf-autosuggest--end-boundary
          (set-marker capf-autosuggest--end-boundary (point))
        (setq capf-autosuggest--end-boundary (point-marker))))
    (delete-overlay overlay)))

(defun capf-autosuggest--post-h ()
  "Create autosuggest overlay.
Remove the actual buffer text prepared by
`capf-autosuggest--pre-h'."
  (when-let* ((end-boundary capf-autosuggest--end-boundary)
              (pos (marker-position end-boundary))
              ((< (point) pos)))
    (delete-region (point) pos)
    (set-marker end-boundary nil))
  (unless completion-in-region-mode
    (pcase (run-hook-wrapped (if (eq capf-autosuggest-capf t)
                                 'completion-at-point-functions
                               'capf-autosuggest-capf)
                             #'completion--capf-wrapper 'all)
      (`(,_fun ,beg ,end ,table . ,plist)
       (if-let* ((completions
                  (let ((capf-autosuggest-all-completions-only-one t))
                    ;; Use `all-completions' rather than
                    ;; `completion-all-completions' to bypass completion
                    ;; styles and perform only prefix completions. This makes
                    ;; sense here as we only use the string without the
                    ;; prefix for the overlay.
                    (all-completions (buffer-substring-no-properties beg end)
                                     table (plist-get plist :predicate)))))
           (progn
             (if capf-autosuggest--overlay
                 (move-overlay capf-autosuggest--overlay end end)
               (setq capf-autosuggest--overlay (make-overlay end end nil t t)))
             (let ((str (substring (car completions) (- end beg))))
               (if (= 0 (length str))
                   (delete-overlay capf-autosuggest--overlay)
                 (setq capf-autosuggest--str (copy-sequence str))
                 (add-text-properties 0 1 (list 'cursor (length str)) str)
                 (add-face-text-property 0 (length str)
                                         'capf-autosuggest-face t str)
                 (overlay-put capf-autosuggest--overlay 'after-string
                              str))))
         (delete-overlay capf-autosuggest--overlay))))))

(define-minor-mode capf-autosuggest-mode
  "Auto-suggest first completion with an overlay."
  :group 'completion
  (if capf-autosuggest-mode
      (progn
        (add-hook 'pre-command-hook #'capf-autosuggest--pre-h nil t)
        (add-hook 'post-command-hook #'capf-autosuggest--post-h nil t))
    (remove-hook 'pre-command-hook #'capf-autosuggest--pre-h t)
    (remove-hook 'post-command-hook #'capf-autosuggest--post-h t)
    (delete-overlay capf-autosuggest--overlay)))

(provide 'capf-autosuggest)
;;; capf-autosuggest.el ends here
