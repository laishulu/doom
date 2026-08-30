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
(add-hook! 'yaml-mode-hook (auto-fill-mode -1))

(setq doom-theme 'doom-one)

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

;; highlight-indent-guides
(setq highlight-indent-guides-suppress-auto-error t)

(map! :g "C-M-s-h" #'evil-window-left
      :g "C-M-s-j" #'evil-window-down
      :g "C-M-s-k" #'evil-window-up
      :g "C-M-s-l" #'evil-window-right)
