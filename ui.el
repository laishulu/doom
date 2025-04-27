;;; ui.el -*- lexical-binding: t; -*-
;; maximize frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; bring frame to front
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (display-graphic-p)
              (let ((frame (selected-frame)))
                (select-frame-set-input-focus frame)))))
;; soft wrap
(global-visual-line-mode)
(blink-cursor-mode)
(global-term-cursor-mode)
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)
(add-hook! 'text-mode-hook 'auto-fill-mode)

(setq doom-theme 'doom-one)

;; DON'T use (`font-family-list'), it's unreliable on Linux
(when (find-font (font-spec :name "Sarasa Term SC Nerd"))
  (setq doom-font (font-spec :family "Sarasa Term SC Nerd" :size 16)
        doom-variable-pitch-font (font-spec :family "Sarasa Term SC Nerd")
        doom-symbol-font (font-spec :family "Sarasa Term SC Nerd")
        doom-big-font (font-spec :family "Sarasa Term SC Nerd" :size 20)))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

;; highlight-indent-guides
(setq highlight-indent-guides-suppress-auto-error t)

(map! :g "C-M-s-h" #'evil-window-left
      :g "C-M-s-j" #'evil-window-down
      :g "C-M-s-k" #'evil-window-up
      :g "C-M-s-l" #'evil-window-right)
