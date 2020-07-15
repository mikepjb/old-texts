;;; darksail-theme --- Summary

;;; Commentary:
;;
;; To discover the font used to color a particular entity in emac:
;; View font-lock under cursor with C-u C-x =
;; Look for 'face' under 'There are text properties here:'


;;; Code:

(deftheme darksail "Dark theme using the default Tailwind color palette.")

(let ((class '((class color) (min-colors 89)))
      (foreground "#F7FAFC")
      (background "#1A202C")
      (cursor-color "#F6AD55")

      (black "#2D3748") ;; 800
      (bright-black "#CBD5E0") ;; 400

      (red "#FC8181") ;; 400
      (bright-red "#FEB2B2") ;; 300

      (green "#68D391")
      (bright-green "#9AE6B4")

      (yellow "#F6E05E")
      (bright-yellow "#FAF089")

      (blue "#63B3ED")
      (bright-blue "#90CDF4")

      (magenta "#F687B3")
      (bright-magenta "#FBB6CE")

      (cyan "#4FD1C5")
      (bright-cyan "#81E6D9")

      (white "#E2E8F0") ;; 300
      (bright-white "#F7FAFC") ;; 100

      (origin-1 "#1d1e1a")
      (origin-2 "#282924")
      (origin-3 "#35372f")
      (origin-4 "#42453b")
      (origin-10 "#eeeeee")
      (origin-9 "#cccccc")
      (magenta-5 "#ff3399")
      (mint-5 "#00ffcc")
      (mint-6 "#66ffcc")
      (aqua-3 "#00cccc")
      (aqua-4 "#00e6e6")
      (aqua-5 "#00ffff")
      (daffodil-5 "#ffff66")
      (lavender-5 "#e6e6ff")
      (darksail-light-grey "#eeeeee")
      (darksail-grey "#444444")
      (darksail-black "#1b1d1e")
      (darksail-white "#eeeeee")
      (darksail-yellow "#cf8f2e")
      (darksail-blue "#486ab4")
      (darksail-green "#365d2e")
      (darksail-magenta "#e13dfc")
      (darksail-purple "#8b008b")
      (darksail-region "#dcb9b9"))

  (custom-theme-set-faces
   'darksail
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,cursor-color))))
   `(region ((t (:background ,black))))
   `(highlight ((t (:background ,black))))
   `(font-lock-builtin-face ((t (:foreground ,green))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-type-face ((t (:foreground ,foreground))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-string-face ((t (:foreground ,bright-yellow))))
   `(font-lock-comment-face ((t (:foreground ,bright-black))))
   `(font-lock-constant-face ((t (:foreground ,red))))

   `(font-lock-variable-name-face ((t (:foreground ,bright-white))))

   `(comint-highlight-prompt ((t (:foreground ,green))))

   `(diff-added ((t (:foreground ,green))))
   `(diff-removed ((t (:foreground ,red))))
   `(diff-hunk-header ((t (:foreground ,green))))
   `(diff-file-header ((t (:foreground ,green))))
   `(diff-header ((t (:foreground ,origin-10))))

   `(ido-first-match ((t (:foreground ,bright-yellow))))
   `(ido-only-match ((t (:foreground ,green))))
   `(ido-subdir ((t (:foreground ,blue))))
   `(ido-indicator ((t (:foreground ,bright-yellow))))

   `(clojure-keyword-face ((t (:foreground ,red))))

   `(sh-quoted-exec ((t (:foreground ,bright-yellow))))

   `(go-test--error-face ((t (:foreground ,red))))

   `(markdown-inline-code-face ((t (:foreground ,bright-yellow))))
   `(markdown-code-face ((t (:foreground ,bright-yellow))))
   `(markdown-italic-face ((t (:slant italic))))

   `(magit-diffstat-added ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
   `(magit-diff-added ((t (:foreground ,green))))
   `(magit-diff-removed ((t (:foreground ,red))))
   `(magit-diff-added-highlight ((t (:foreground ,green))))
   `(magit-diff-removed-highlight ((t (:foreground ,red))))

   `(trailing-whitespace ((t (:background ,magenta))))

   `(mode-line
     ((,class (:foreground ,cyan :background ,black :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive
     ((,class (:foreground ,white :background ,black :box (:line-width -1 :style released-button)))))

   ;; magit-diff-added-highlight

   `(match ((t (:foreground ,black :background ,bright-yellow)))) ;; used in ripgrep
   `(compilation-info ((t (:foreground ,origin-10))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,aqua-5))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,aqua-4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua-3))))
   `(company-tooltip ((t (:foreground ,origin-10 :background ,origin-2))))
   `(company-tooltip-selection ((t (:foreground ,origin-10 :background ,origin-3))))
   `(company-tooltip-annotation ((t (:foreground ,aqua-3))))
   `(company-tooltip-annotation-selection ((t (:foreground ,aqua-5))))
   `(company-tooltip-common ((t (:foreground ,aqua-3))))
   `(company-tooltip-common-selection ((t (:foreground ,magenta-5))))
   `(company-scrollbar-fg ((t (:background ,origin-9))))
   `(company-scrollbar-bg ((t (:background ,origin-1))))
   `(company-preview ((t (:foreground ,origin-9 :background ,origin-2))))
   `(company-preview-common ((t (:foreground ,origin-9 :background ,origin-2))))
   (custom-theme-set-variables
    'darksail
    `(ansi-color-names-vector
      [,black
       ,red
       ,green
       ,yellow
       ,blue
       ,magenta
       ,blue
       ,white]))
   `(minibuffer-prompt ((,class (:foreground ,origin-10))))
   ))

(provide-theme 'darksail)
;;; lumo-theme.el ends here
