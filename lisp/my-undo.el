;;; my-undo.el --- Undo tree configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :bind
  ("C-x u" . undo-tree-visualize))

(provide 'my-undo)
;;; my-undo.el ends here
