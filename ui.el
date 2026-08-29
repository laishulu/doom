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

(defun my/apply-custom-fonts (&optional frame)
  "Apply custom fonts and CJK fontsets to graphical frames."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let ((the-font-family "Sarasa Term SC Nerd"))
        ;; Ensure the target font is installed and available
        (when (find-font (font-spec :family the-font-family))
          ;; Set standard Doom font variables
          (setq doom-font (font-spec :family the-font-family :size 16)
                doom-variable-pitch-font (font-spec :family the-font-family)
                doom-symbol-font (font-spec :family the-font-family)
                doom-big-font (font-spec :family the-font-family :size 20))
          ;; Configure CJK fallback fontsets
          (dolist (charset '(han cjk-misc bopomofo))
            (set-fontset-font t charset (font-spec :family the-font-family)))
          ;; Force Doom to apply font changes to the active frame
          (doom/reload-font))))))

;; Apply fonts when creating a new frame via emacsclient
(add-hook 'after-make-frame-functions #'my/apply-custom-fonts)

;; Re-apply fonts after switching or reloading themes
(add-hook 'doom-after-load-theme-hook #'my/apply-custom-fonts)

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

;; highlight-indent-guides
(setq highlight-indent-guides-suppress-auto-error t)

(map! :g "C-M-s-h" #'evil-window-left
      :g "C-M-s-j" #'evil-window-down
      :g "C-M-s-k" #'evil-window-up
      :g "C-M-s-l" #'evil-window-right)
