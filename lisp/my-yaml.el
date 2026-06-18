;;; my-yaml.el --- YAML mode configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(provide 'my-yaml)
;;; my-yaml.el ends here
