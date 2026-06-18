;;; my-iedit.el --- Iedit configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(provide 'my-iedit)
;;; my-iedit.el ends here
