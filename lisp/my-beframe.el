;;; my-beframe.el --- Beframe configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package beframe
  :ensure t
  :config
  (beframe-mode 1)
  (with-eval-after-load 'my-completion
    (setq consult-buffer-list-function #'my/consult-beframe-buffer-list)))

(provide 'my-beframe)
;;; my-beframe.el ends here
