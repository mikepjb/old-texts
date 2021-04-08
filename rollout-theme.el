(deftheme rollout "Code illuminated.")

(let ((class '((class color) (min-colors 89)))
      (background "#1a202c")
      (foreground "#f7fafc")
      (cursor "#f7fafc")
      (black "#2d3748")
      (bright-black "#718096")
      (red "#f56565")
      (bright-red "#feb2b2")
      (green "#c6f6d5")
      (bright-green "#f0fff4")
      (yellow "#faf089")
      (bright-yellow "#fefcbf")
      (blue "#bee3f8")
      (bright-blue "#ebfff8")
      (magenta "#ed64a6")
      (bright-magenta "#fbb6ce")
      (cyan "#4fd1c5")
      (bright-cyan "#b2f5ea")
      (white "#f7fafc")
      (bright-white "#ffffff"))

  ;; view font-lock under cursor with C-u C-x =
  (custom-theme-set-faces
   'rollout
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,magenta))))
   `(region ((t (:background ,black))))
   `(highlight ((t (:background ,black))))
   `(parenthesis ((t (:foreground ,cyan)))) ;; uses paren-face package
   `(font-lock-builtin-face ((t (:foreground ,cyan))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-type-face ((t (:foreground ,red))))
   `(font-lock-function-name-face ((t (:foreground ,yellow))))
   `(font-lock-string-face ((t (:foreground ,white))))
   `(font-lock-comment-face ((t (:foreground ,bright-black))))
   `(comint-highlight-prompt ((t (:foreground ,blue))))
   `(diff-added ((t (:foreground ,green))))
   `(diff-removed ((t (:foreground ,red))))
   `(diff-hunk-header ((t (:foreground ,magenta))))
   `(diff-file-header ((t (:foreground ,yellow))))
   `(diff-header ((t (:foreground ,cyan))))

   `(clojure-keyword-face ((t (:foreground ,red))))

   `(markdown-inline-code-face ((t (:foreground ,red))))

   `(ido-first-match ((t (:foreground ,cyan))))
   `(ido-only-match ((t (:foreground ,green))))
   `(ido-subdir ((t (:foreground ,yellow))))
   `(ido-indicator ((t (:foreground ,red))))

   `(mode-line
     ((t (:background ,white
                      :foreground ,black
                      :box (:line-width -1 :color ,bright-black)))))
   `(mode-line-inactive
     ((t (:background ,white
                      :foreground ,black
                      :box (:line-width -1 :color ,green)))))

   `(match ((t (:background ,yellow))))
   `(compilation-info ((t (:foreground ,yellow))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,bright-cyan))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,magenta))))

   `(company-tooltip ((t (:foreground ,bright-black :background ,black))))
   `(company-tooltip-selection ((t (:foreground ,black :background ,cyan))))
   `(company-tooltip-annotation ((t (:foreground ,bright-black))))
   `(company-tooltip-annotation-selection ((t (:foreground ,bright-black))))
   `(company-tooltip-common ((t (:foreground ,green))))
   `(company-tooltip-common-selection ((t (:foreground ,black))))
   `(company-scrollbar-fg ((t (:background ,bright-black))))
   `(company-scrollbar-bg ((t (:background ,black))))
   `(company-preview ((t (:foreground ,bright-black :background ,red))))
   `(company-preview-common ((t (:foreground ,bright-black :background ,yellow))))
   (custom-theme-set-variables
    'rollout
    `(ansi-color-names-vector
      [,black
       ,red
       ,green
       ,yellow
       ,blue
       ,magenta
       ,blue
       ,white]))
   `(minibuffer-prompt ((,class (:foreground ,cyan))))
   ))

(provide-theme 'rollout)
