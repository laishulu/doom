;;; ui.el -*- lexical-binding: t; -*-
;; 1. Set global font variables directly at top level (works for both direct startup and daemon)
(let ((the-font-family "Sarasa Term SC Nerd"))
  (setq doom-font (font-spec :family the-font-family :size 16)
        doom-variable-pitch-font (font-spec :family the-font-family)
        doom-symbol-font (font-spec :family the-font-family)
        doom-big-font (font-spec :family the-font-family :size 20)))

;; 2. CJK fallback setup for GUI frames
(defun my/apply-cjk-fonts (&optional frame)
  "Apply CJK fallback fontsets to graphical frames."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let ((the-font-family "Sarasa Term SC Nerd"))
        (when (find-font (font-spec :family the-font-family))
          (dolist (charset '(han cjk-misc bopomofo))
            (set-fontset-font t charset (font-spec :family the-font-family) frame)))))))

;; Direct standalone Emacs GUI startup
(add-hook 'doom-init-ui-hook #'my/apply-cjk-fonts)

;; Emacsclient new frames
(add-hook 'after-make-frame-functions #'my/apply-cjk-fonts)

;; Theme reload/switch
(add-hook 'doom-after-load-theme-hook #'my/apply-cjk-fonts)
