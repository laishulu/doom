(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default c-basic-offset 2)

;; disable META key sequence of interpreting ESC to META
(global-set-key [escape] 'keyboard-escape-quit)

(setq confirm-kill-processes nil)

(use-package! sis
  :after (evil tmux-pane)
  :config
  (sis-ism-lazyman-config
    "com.apple.keylayout.US"
    "org.fcitx.inputmethod.Fcitx5.fcitx5")
  (delete "C-h" sis-prefix-override-keys)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)
  (sis-global-context-mode t)
  (sis-global-cursor-color-mode t))
