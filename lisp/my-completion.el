;;; my-completion.el --- Minibuffer and completion configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package orderless
  :ensure t
  :init
  ;; Allows space-separated completion terms to match in any order, which
  ;; makes commands like `M-x' and `consult-buffer' much more flexible.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  ;; Provides the vertical minibuffer candidate UI used by completion commands.
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; Improves directory navigation during file completion prompts.
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

(use-package marginalia
  :ensure t
  ;; Adds right-side annotations in minibuffer completion lists, such as
  ;; buffer modes/paths for `consult-buffer' and extra metadata for `M-x'.
  :init
  (marginalia-mode))

(provide 'my-completion)
;;; my-completion.el ends here
