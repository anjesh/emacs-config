;;; my-eglot.el --- Eglot configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package eglot
  :ensure nil
  :hook (python-mode . eglot-ensure)
  :bind (:map python-mode-map
              ("C-c C-d" . eldoc)
              ([C-down-mouse-1] . xref-find-definitions-at-mouse))
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(provide 'my-eglot)
;;; my-eglot.el ends here
