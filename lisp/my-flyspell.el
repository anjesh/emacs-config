;;; my-flyspell.el --- Flyspell configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/toggle-spell-check ()
  "Toggle flyspell-mode on or off."
  (interactive)
  (require 'flyspell)
  (if (bound-and-true-p flyspell-mode)
      (progn
        (flyspell-mode -1)
        (message "Spell check: OFF"))
    (flyspell-mode 1)
    (message "Spell check: ON")))

(use-package flyspell
  :ensure nil
  :defer t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (("C-c s C" . my/toggle-spell-check)
         ("C-c s c" . flyspell-correct-word-before-point))
  :config
  (cond
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra")))))

(provide 'my-flyspell)
;;; my-flyspell.el ends here
