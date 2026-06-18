;;; my-corfu.el --- Corfu configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/corfu-enable-orderless ()
  "Use orderless completion styles in Corfu buffers."
  (setq-local completion-styles '(orderless basic)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  :config
  (add-hook 'corfu-mode-hook #'my/corfu-enable-orderless)
  (use-package corfu-terminal
    :ensure t
    :if (and (not (display-graphic-p))
             (< emacs-major-version 31))
    :config
    (corfu-terminal-mode +1)))

(provide 'my-corfu)
;;; my-corfu.el ends here
