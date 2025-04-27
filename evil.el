;;; evil.el -*- lexical-binding: t; -*-
(after! evil
  (setq evil-kill-on-visual-paste nil))

;; fix incompatibility with org so RET can also call the command
(defadvice! fixed-org/dwim-at-point (fn &rest args)
  :around #'+org/dwim-at-point
  (if (and (bound-and-true-p iedit-mode)
           (iedit-current-occurrence-string))
      (ignore (call-interactively #'evil-multiedit-toggle-or-restrict-region))
    (apply fn args)))

(use-package! evil-pinyin
  :after (evil)
  :init
  (setq-default evil-pinyin-scheme
                ;; 采用小鹤双拼码表
                'simplified-xiaohe-all)
  :config
  (global-evil-pinyin-mode t))

(use-package! evil-textobj-column
  :config
  (define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
  (define-key evil-outer-text-objects-map "c" 'evil-textobj-column-word)
  (define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)
  (define-key evil-outer-text-objects-map "C" 'evil-textobj-column-WORD))

(use-package! evil-textobj-syntax)

(use-package! evil-textobj-line)

(use-package! evil-textobj-entire
  :config
  (define-key evil-outer-text-objects-map
    evil-textobj-entire-key 'evil-entire-entire-buffer)
  (define-key evil-inner-text-objects-map
    evil-textobj-entire-key 'evil-entire-entire-buffer))

;;;###autoload
(defun replace-current-line ()
  "Replace the current line."
  (interactive)
  (kill-region (line-beginning-position) (line-end-position))
  (insert (string-trim (current-kill 1)))
  (indent-according-to-mode)
  (back-to-indentation))

(map! :nv "gl" #'replace-current-line)
