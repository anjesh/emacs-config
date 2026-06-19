;;; my-completion.el --- Minibuffer and completion configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-k" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(provide 'my-completion)
;;; my-completion.el ends here
