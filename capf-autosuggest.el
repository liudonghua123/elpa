;;; capf-autosuggest.el --- Show first completion-at-point as an overlay -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'subr-x))

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

;;;###autoload
(defmacro capf-autosuggest-define-partial-accept-cmd (name command)
  "Define a command NAME.
It will call COMMAND interactively, allowing it to move point
into an auto-suggested overlay. COMMAND must not modify buffer.
NAME must not be called if `capf-autosuggest-active-mode' in
inactive. Usually, NAME is bound in
`capf-autosuggest-active-mode-map'."
  `(defun ,name ()
     ,(format "`%s', possibly moving point into an auto-suggested overlay."
              command)
     (interactive)
     (capf-autosuggest-call-partial-accept-cmd #',command)))

(defun capf-autosuggest-call-partial-accept-cmd (command)
  "Call COMMAND interactively, stepping into auto-suggested overlay.
Temporarily convert the overlay to buffer text and call COMMAND
interactively. Afterwards, the added text is deleted, but only
the portion after point. Additionally, if the point is outside of
the added text, the whole text is deleted."
  (let (beg end text)
    (with-silent-modifications
      (catch 'cancel-atomic-change
        (atomic-change-group
          (save-excursion
            (goto-char (overlay-start capf-autosuggest--overlay))
            (setq beg (point))
            (insert capf-autosuggest--str)
            (setq end (point)))
          (call-interactively command)
          (and (> (point) beg)
               (<= (point) end)
               (setq text (buffer-substring beg (point))))
          (throw 'cancel-atomic-change nil))))
    (when text
      (if (= (point) beg)
          (insert text)
        (save-excursion
          (goto-char beg)
          (insert text))))))

(defvar capf-autosuggest-active-mode)

(defun capf-autosuggest--post-h ()
  "Create an auto-suggest overlay.
Remove text inserted by `capf-autosuggest--pre-h', but only from
point forward."
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
         (let* ((pred (plist-get plist :predicate))
                (string (buffer-substring-no-properties beg end))
                (base (car (completion-boundaries string table pred ""))))
           (when-let*
               ((completions
                 (let ((capf-autosuggest-all-completions-only-one t))
                   ;; Use `all-completions' rather than
                   ;; `completion-all-completions' to bypass completion styles
                   ;; and strictly match only on prefix. This makes sense here
                   ;; as we only use the string without the prefix for the
                   ;; overlay.
                   (all-completions string table pred)))
                (str (substring (car completions) (- end beg base)))
                ((/= 0 (length str))))
             (setq capf-autosuggest--region (cons beg end)
                   capf-autosuggest--str (copy-sequence str))
             (move-overlay capf-autosuggest--overlay end end)
             (add-text-properties 0 1 (list 'cursor (length str)) str)
             (add-face-text-property 0 (length str) 'capf-autosuggest-face t str)
             (overlay-put capf-autosuggest--overlay 'after-string str)
             (capf-autosuggest-active-mode))))))))

;;;###autoload
(define-minor-mode capf-autosuggest-mode
  "Auto-suggest first completion at point with an overlay."
  :group 'completion
  (if capf-autosuggest-mode
      (progn
        (setq capf-autosuggest--overlay (make-overlay (point) (point) nil t t))
        (add-hook 'post-command-hook #'capf-autosuggest--post-h nil t)
        (add-hook 'completion-in-region-mode-hook #'capf-autosuggest--post-h nil t))
    (remove-hook 'post-command-hook #'capf-autosuggest--post-h t)
    (remove-hook 'completion-in-region-mode-hook #'capf-autosuggest--post-h t)
    (capf-autosuggest-active-mode -1)))

(declare-function evil-forward-char "ext:evil-commands" nil t)
(declare-function evil-end-of-line "ext:evil-commands" nil t)
(declare-function evil-end-of-visual-line "ext:evil-commands" nil t)
(declare-function evil-end-of-line-or-visual-line "ext:evil-commands" nil t)
(declare-function evil-middle-of-visual-line "ext:evil-commands" nil t)
(declare-function evil-last-non-blank "ext:evil-commands" nil t)
(declare-function evil-forward-word-begin "ext:evil-commands" nil t)
(declare-function evil-forward-word-end "ext:evil-commands" nil t)
(declare-function evil-forward-WORD-begin "ext:evil-commands" nil t)
(declare-function evil-forward-WORD-end "ext:evil-commands" nil t)

(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-forward-word forward-word)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-forward-char forward-char)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-end-of-line end-of-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-move-end-of-line move-end-of-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-end-of-visual-line end-of-visual-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-forward-char evil-forward-char)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-end-of-line evil-end-of-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-end-of-visual-line evil-end-of-visual-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-end-of-line-or-visual-line evil-end-of-line-or-visual-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-middle-of-visual-line evil-middle-of-visual-line)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-last-non-blank evil-last-non-blank)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-forward-word-begin evil-forward-word-begin)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-forward-word-end evil-forward-word-end)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-forward-WORD-begin evil-forward-WORD-begin)
(capf-autosuggest-define-partial-accept-cmd capf-autosuggest-evil-forward-WORD-end evil-forward-WORD-end)

(defvar capf-autosuggest-active-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'capf-autosuggest-forward-word)
    (define-key map [remap forward-char] #'capf-autosuggest-forward-char)
    (define-key map [remap end-of-line] #'capf-autosuggest-end-of-line)
    (define-key map [remap move-end-of-line] #'capf-autosuggest-move-end-of-line)
    (define-key map [remap end-of-visual-line] #'capf-autosuggest-end-of-visual-line)
    (define-key map [remap evil-forward-char] #'capf-autosuggest-evil-forward-char)
    (define-key map [remap evil-end-of-line] #'capf-autosuggest-evil-end-of-line)
    (define-key map [remap evil-end-of-visual-line] #'capf-autosuggest-evil-end-of-visual-line)
    (define-key map [remap evil-end-of-line-or-visual-line] #'capf-autosuggest-evil-end-of-line-or-visual-line)
    (define-key map [remap evil-middle-of-visual-line] #'capf-autosuggest-evil-middle-of-visual-line)
    (define-key map [remap evil-last-non-blank] #'capf-autosuggest-evil-last-non-blank)
    (define-key map [remap evil-forward-word-begin] #'capf-autosuggest-evil-forward-word-begin)
    (define-key map [remap evil-forward-word-end] #'capf-autosuggest-evil-forward-word-end)
    (define-key map [remap evil-forward-WORD-begin] #'capf-autosuggest-evil-forward-WORD-begin)
    (define-key map [remap evil-forward-WORD-end] #'capf-autosuggest-evil-forward-WORD-end)
    map)
  "Keymap active when an auto-suggestion is shown.")

(defun capf-autosuggest-accept ()
  "Accept current auto-suggestion."
  (interactive)
  (capf-autosuggest-call-partial-accept-cmd
   (lambda ()
     (interactive)
     (goto-char (overlay-start capf-autosuggest--overlay)))))

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
