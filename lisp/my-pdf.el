;;; my-pdf.el --- PDF tools configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-continuous t)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (pdf-isearch-minor-mode)
              (auto-revert-mode 1))))

(provide 'my-pdf)
;;; my-pdf.el ends here
