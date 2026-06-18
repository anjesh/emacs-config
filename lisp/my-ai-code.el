;;; my-ai-code.el --- AI Code configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package ai-code
  :ensure t
  :bind (("C-c i" . ai-code-menu)))

(provide 'my-ai-code)
;;; my-ai-code.el ends here
