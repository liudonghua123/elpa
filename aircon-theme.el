;;; aircon-theme.el --- Cool and legible light theme

;; Copyright (C) 2021 Gregory Chamberlain.

;; Author: Gregory Chamberlain <greg@cosine.blue>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(deftheme aircon
  "Cool and legible light theme.")

(let ((c '((class color) (min-colors 89)))
      (error "#8d2934")
      (warning "#b88325")
      (success "#468b16"))
 (custom-theme-set-faces
  'aircon
  
  `(default             ((,c (:background "#ffffff" :foreground "#000000"))))
  `(cursor              ((,c (:background "#243867"))))

  `(fringe              ((,c (:inherit default))))

  `(mode-line           ((,c (:inherit variable-pitch
                              :background "#49587b"
                              :foreground "#ffffff"
                              :box "#010e2c"))))
  `(mode-line-inactive  ((,c (:inherit mode-line
                              :background "#e2e3ea"
                              :foreground "#3a4254"
                              :box "#a3a4ae"))))
  `(mode-line-highlight ((,c (:inherit mode-line))))

  `(compilation-mode-line-exit (()))
  `(compilation-mode-line-fail ((,c :foreground "#cc5555" :inherit bold)))
  `(compilation-error          ((,c :foreground "#cc5555" :inherit bold)))
  `(compilation-info           ((,c :foreground "#669922" :inherit bold)))
  `(compilation-warning        ((,c :foreground "#c9a830" :inherit bold)))

  `(shadow       ((,c (:foreground "#595959"))))
  `(error        ((,c (:foreground ,error   :inherit bold))))
  `(warning      ((,c (:foreground ,warning :inherit bold))))
  `(success      ((,c (:foreground ,success :inherit bold))))

  `(escape-glyph   ((,c (:inherit error))))
  `(homoglyph      ((,c (:inherit error))))

  `(highlight           ((,c (:background "#c4cbdc"))))
  `(region              ((,c (:background "#e2e3ea"))))
  `(secondary-selection ((,c (:background "#d5cded"))))
  `(isearch             ((,c (:background "#ffcb63" :foreground "#422c00"))))
  `(isearch-fail        ((,c (:background "#eea5a7"))))
  `(lazy-highlight      ((,c (:background "#f7e8ca"))))
  `(show-paren-match    ((,c (:inherit isearch))))
  `(show-paren-mismatch ((,c (:inherit isearch-fail))))
  `(show-paren-match-expression ((,c (:inherit lazy-highlight))))

  `(font-lock-builtin-face       ((,c (:foreground "#243867" :inherit bold))))
  `(font-lock-comment-face       ((,c (:foreground "#533a09" :inherit italic))))
  `(font-lock-constant-face      ((,c (:foreground "#307493"))))
  `(font-lock-function-name-face ((,c (:foreground "#412f7e"))))
  `(font-lock-keyword-face       ((,c (:foreground "#2e4d98" :inherit bold))))
  `(font-lock-string-face        ((,c (:foreground "#327038"))))
  `(font-lock-type-face          ((,c (:foreground "#9a2d71" :inherit bold))))
  `(font-lock-variable-name-face ((,c (:foreground "#5f4f93"))))
  `(font-lock-warning-face       ((,c (:inherit error))))
  `(font-lock-doc-face           ((,c (:inherit (font-lock-string-face
                                                 italic)))))

  `(flyspell-duplicate ((,c (:underline (:style wave :color ,warning)))))
  `(flyspell-incorrect ((,c (:underline (:style wave :color ,error)))))

  `(link         ((,c (:inherit underline :foreground "#2e4d98"))))
  `(link-visited ((,c (:inherit link      :foreground "#5f4f93"))))

  `(minibuffer-prompt   ((,c (:inherit font-lock-builtin-face))))

  `(completions-common-part      ((,c (:inherit lazy-highlight))))
  `(completions-first-difference ((,c (:inherit isearch))))

  `(diff-added           ((,c (:background "#c4face" :foreground "#143c1d"))))
  `(diff-refine-added    ((,c (:background "#88cd98" :foreground "#0c2912 "))))
  `(diff-removed         ((,c (:background "#ffcccc" :foreground "#4b1313"))))
  `(diff-refine-removed  ((,c (:background "#ef9d9d" :foreground "#3a0a0a "))))

  `(dired-directory      ((,c (:inherit font-lock-builtin-face))))
  `(dired-header         ((,c (:inherit font-lock-builtin-face))))
  `(dired-symlink        ((,c (:inherit font-lock-constant-face))))
  `(dired-ignored        ((,c (:inherit shadow))))
  `(dired-mark           ((,c (:inherit isearch :background "#ffffff"))))
  `(dired-marked         ((,c (:inherit isearch))))
  `(dired-flagged        ((,c (:inherit isearch-fail))))

  `(eshell-ls-archive    ((,c (:inherit font-lock-type-face))))
  `(eshell-ls-backup     ((,c (:inherit shadow))))
  `(eshell-ls-directory  ((,c (:inherit font-lock-builtin-face))))
  `(eshell-ls-executable ((,c (:inherit font-lock-string-face))))
  `(eshell-ls-missing    ((,c (:inherit error))))
  `(eshell-ls-readonly   ((,c (:inherit font-lock-comment-face))))
  `(eshell-ls-symlink    ((,c (:inherit font-lock-constant-face))))
  `(eshell-prompt        ((,c (:inherit font-lock-builtin-face))))

  ))

(custom-theme-set-variables
 'aircon
 '(ansi-color-names-vector ["#ffffff" "#9a2d71" "#327038" "#18566e"
			    "#2e4d98" "#5f4f93" "#533a09" "#243867"]))

(provide-theme 'aircon)

;;; aircon-theme.el ends here
