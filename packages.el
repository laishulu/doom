;;; packages.el -*- lexical-binding: t; -*-
;;; -*- no-byte-compile: t; -*-
(package! evil-textobj-column)
(package! evil-textobj-line)
(package! evil-textobj-syntax)
(package! evil-textobj-entire)
(package! sis)
(package! evil-pinyin)
(package! term-cursor :recipe
  (:host github :repo "h0d/term-cursor.el"))
;; word around to the session restore issue
(package! treemacs-persp :disable t)
(package! command-log-mode)
