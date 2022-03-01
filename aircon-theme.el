;;; aircon-theme.el --- Cool and legible light theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Gregory Chamberlain.

;; Version: 0.0.2
;; Author: Gregory Chamberlain <greg@cosine.blue>
;; URL: https://git.sr.ht/~chambln/aircon-theme.el
;; Keywords: faces
;; Package-Requires: ((emacs "24.4"))

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

;;; Commentary:

;; Aircon is a clean and high contrast custom theme for Emacs. It
;; consists of a white (#ffffff) background and mostly blues, purples
;; and greens. Highlights are subtle but easily visible. The active
;; mode-line is white on blue to distinguish it from inactive
;; mode-lines which are dark blue on light grey.

;;; Code:

(deftheme aircon "Cool and legible light theme.")

(defconst aircon-brick     "#8d2934")
(defconst aircon-marigold  "#b88325")
(defconst aircon-hibiscus  "#9a2d71")
(defconst aircon-sapphire  "#2e4d98")
(defconst aircon-cello     "#243867")
(defconst aircon-eden      "#18566e")
(defconst aircon-lilac     "#5f4f93")
(defconst aircon-goblin    "#327038")
(defconst aircon-sandstone "#615855")
(defconst aircon-midnight  "#010e2c")
(defconst aircon-shark     "#303645")
(defconst aircon-ghost     "#a3a4ae")

(defface aircon-scorpion
  '((((class color) (min-colors 89)) (:foreground "#595959")))
  "Aircon scorpion.")
(defface aircon-brick
  `((((class color) (min-colors 89)) (:foreground ,aircon-brick)))
  "Aircon brick.")
(defface aircon-marigold
  `((((class color) (min-colors 89)) (:foreground ,aircon-marigold)))
  "Aircon marigold.")
(defface aircon-hibiscus
  `((((class color) (min-colors 89)) (:foreground ,aircon-hibiscus)))
  "Aircon hibiscus.")
(defface aircon-sapphire
  `((((class color) (min-colors 89)) (:foreground ,aircon-sapphire)))
  "Aircon sapphire.")
(defface aircon-cello
  `((((class color) (min-colors 89)) (:foreground ,aircon-cello)))
  "Aircon cello.")
(defface aircon-eden
  `((((class color) (min-colors 89)) (:foreground ,aircon-eden)))
  "Aircon eden.")
(defface aircon-grape
  '((((class color) (min-colors 89)) (:foreground "#412f7e")))
  "Aircon grape.")
(defface aircon-lilac
  `((((class color) (min-colors 89)) (:foreground ,aircon-lilac)))
  "Aircon lilac.")
(defface aircon-goblin
  `((((class color) (min-colors 89)) (:foreground ,aircon-goblin)))
  "Aircon goblin.")
(defface aircon-sandstone
  `((((class color) (min-colors 89)) (:foreground ,aircon-sandstone)))
  "Aircon sandstone.")
(defface aircon-haze
  '((((class color) (min-colors 89)) (:background "#c4cbdc")))
  "Aircon haze.")
(defface aircon-blush
  '((((class color) (min-colors 89)) (:background "#eea5a7")))
  "Aircon blush.")
(defface aircon-prelude
  '((((class color) (min-colors 89)) (:background "#d5cded")))
  "Aircon prelude.")
(defface aircon-fjord
  '((((class color) (min-colors 89)) (:background "#49587b")))
  "Aircon fjord.")
(defface aircon-athens
  '((((class color) (min-colors 89)) (:background "#e2e3ea")))
  "Aircon athens.")
(defface aircon-linen
  '((((class color) (min-colors 89)) (:background "#f7e8ca")))
  "Aircon linen.")
(defface aircon-mango
  '((((class color) (min-colors 89))
     (:background "#ffcb63" :foreground "#422c00")))
  "Aircon mango.")
(defface aircon-header
  `((nil (:inherit (aircon-athens) :foreground ,aircon-shark)))
  "Aircon header.")

(custom-theme-set-faces
 'aircon

 `(ansi-color-black   ((nil (:foreground "#000000"))))
 `(ansi-color-white   ((nil (:foreground "#ffffff"))))
 `(ansi-color-blue    ((nil (:foreground ,aircon-sapphire))))
 `(ansi-color-cyan    ((nil (:foreground ,aircon-eden))))
 `(ansi-color-green   ((nil (:foreground ,aircon-goblin))))
 `(ansi-color-magenta ((nil (:foreground ,aircon-hibiscus))))
 `(ansi-color-red     ((nil (:foreground ,aircon-brick))))
 `(ansi-color-yellow  ((nil (:foreground ,aircon-marigold))))

 '(default ((nil (:background "#ffffff" :foreground "#000000"))))
 `(cursor  ((nil (:background ,aircon-cello))))
 '(fringe  ((nil (:inherit (default)))))

 `(mode-line                  ((nil (:inherit (variable-pitch aircon-fjord)
                                     :foreground "#ffffff"
                                     :box ,aircon-midnight))))
 `(mode-line-inactive         ((nil (:inherit (aircon-header mode-line)
                                              :box ,aircon-ghost))))
 '(mode-line-highlight        ((nil (:inherit (mode-line)))))

 '(compilation-mode-line-fail ((nil (:inherit (bold aircon-brick)))))
 '(compilation-error          ((nil (:inherit (bold aircon-brick)))))


 `(help-key-binding ((nil (:inherit (aircon-header)
                           :box (:color ,aircon-ghost :line-width (1 . -1))))))

 '(shadow       ((nil (:inherit (aircon-scorpion)))))
 '(error        ((nil (:inherit (bold aircon-brick)))))
 '(warning      ((nil (:inherit (bold aircon-marigold)))))
 '(success      ((nil (:inherit (bold aircon-goblin)))))

 '(escape-glyph   ((nil (:inherit (aircon-brick)))))
 '(homoglyph      ((nil (:inherit (aircon-brick)))))

 '(highlight           ((nil (:inherit (aircon-haze)))))
 '(region              ((nil (:inherit (aircon-athens)))))
 '(secondary-selection ((nil (:inherit (aircon-prelude)))))
 '(isearch             ((nil (:inherit (aircon-mango)))))
 '(isearch-fail        ((nil (:inherit (aircon-blush)))))
 '(lazy-highlight      ((nil (:inherit (aircon-linen)))))
 '(tooltip             ((nil (:inherit (aircon-linen)))))
 '(show-paren-match    ((nil (:inherit (aircon-mango)))))
 '(show-paren-mismatch ((nil (:inherit (aircon-blush)))))
 '(show-paren-match-expression ((nil (:inherit (aircon-linen)))))

 '(font-lock-builtin-face       ((nil (:inherit (aircon-cello bold)))))
 '(font-lock-comment-face       ((nil (:inherit (aircon-sandstone italic)))))
 '(font-lock-constant-face      ((nil (:inherit (aircon-eden)))))
 '(font-lock-function-name-face ((nil (:inherit (aircon-grape)))))
 '(font-lock-keyword-face       ((nil (:inherit (aircon-sapphire bold)))))
 '(font-lock-string-face        ((nil (:inherit (aircon-goblin)))))
 '(font-lock-type-face          ((nil (:inherit (aircon-hibiscus bold)))))
 '(font-lock-variable-name-face ((nil (:inherit (aircon-lilac)))))
 '(font-lock-warning-face       ((nil (:inherit (aircon-brick)))))
 '(font-lock-doc-face           ((nil (:inherit (italic aircon-goblin)))))

 '(link                         ((nil (:inherit (aircon-sapphire underline)))))
 '(link-visited                 ((nil (:inherit (aircon-lilac link)))))

 '(minibuffer-prompt            ((nil (:inherit (aircon-cello bold)))))

 '(completions-common-part      ((nil (:inherit (aircon-linen)))))
 '(completions-first-difference ((nil (:inherit (aircon-mango)))))

 `(flyspell-duplicate ((nil (:underline (:color ,aircon-marigold :style wave)))))
 `(flyspell-incorrect ((nil (:underline (:color ,aircon-brick :style wave)))))

 '(diff-header          ((nil (:inherit (aircon-header)))))
 '(diff-file-header     ((nil (:inherit (diff-header) :weight bold))))
 '(diff-added           ((nil (:background "#c4face" :foreground "#143c1d"))))
 '(diff-refine-added    ((nil (:background "#88cd98" :foreground "#0c2912 "))))
 '(diff-removed         ((nil (:background "#ffcccc" :foreground "#4b1313"))))
 '(diff-refine-removed  ((nil (:background "#ef9d9d" :foreground "#3a0a0a "))))

 '(magit-diff-added              ((nil (:inherit diff-added))))
 '(magit-diff-added-highlight    ((nil (:inherit diff-refine-added))))
 '(magit-diff-removed            ((nil (:inherit diff-removed))))
 '(magit-diff-removed-highlight  ((nil (:inherit diff-refine-removed))))
 `(magit-diff-whitespace-warning ((nil (:background ,aircon-hibiscus))))

 '(dired-directory      ((nil (:inherit (aircon-cello bold)))))
 '(dired-header         ((nil (:inherit (dired-directory)))))
 '(dired-symlink        ((nil (:inherit (aircon-eden)))))
 '(dired-broken-symlink ((nil (:inherit (aircon-brick)))))
 '(dired-ignored        ((nil (:inherit (shadow)))))
 '(dired-mark           ((nil (:inherit (aircon-mango) :background "#ffffff"))))
 '(dired-marked         ((nil (:inherit (aircon-mango)))))
 '(dired-flagged        ((nil (:inherit (aircon-blush)))))

 '(eshell-ls-archive    ((nil (:inherit aircon-hibiscus))))
 '(eshell-ls-backup     ((nil (:inherit shadow))))
 '(eshell-ls-directory  ((nil (:inherit aircon-cello))))
 '(eshell-ls-executable ((nil (:inherit aircon-goblin))))
 '(eshell-ls-missing    ((nil (:inherit error))))
 '(eshell-ls-readonly   ((nil (:inherit aircon-sandstone))))
 '(eshell-ls-symlink    ((nil (:inherit aircon-eden))))
 '(eshell-prompt        ((nil (:inherit aircon-cello))))

 '(erc-pal-face            ((nil (:inherit aircon-hibiscus :weight bold))))
 '(erc-button              ((nil (:inherit button))))
 '(erc-keyword-face        ((nil (:inherit aircon-sapphire))))
 '(erc-current-nick-face   ((nil (:inherit aircon-sapphire))))
 '(erc-dangerous-host-face ((nil (:inherit warning))))
 '(erc-direct-msg-face     ((nil (:inherit aircon-cello :weight normal))))
 '(erc-error-face          ((nil (:inherit error))))
 '(erc-input-face          ((nil (:inherit aircon-lilac))))
 '(erc-nick-default-face   ((nil (:inherit aircon-sapphire :weight bold))))
 '(erc-nick-msg-face       ((nil (:inherit aircon-sapphire :weight bold))))
 '(erc-notice-face         ((nil (:inherit aircon-sandstone))))
 '(erc-prompt-face         ((nil (:inherit minibuffer-prompt))))
 '(erc-timestamp-face      ((nil (:inherit aircon-eden))))

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aircon)
(provide 'aircon-theme)

;;; aircon-theme.el ends here
