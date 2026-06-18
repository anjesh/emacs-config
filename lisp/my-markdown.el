;;; my-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  :hook (markdown-mode . (lambda ()
                           (font-lock-mode 1)
                           (visual-line-mode 1)))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-preview-mode
  :ensure t
  :defer t
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css")))

(provide 'my-markdown)
;;; my-markdown.el ends here
