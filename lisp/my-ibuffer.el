;;; my-ibuffer.el --- Ibuffer configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package ibuffer
  :ensure t
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-buffers nil)
  :config
  (setq ibuffer-formats
        '((mark
           (name 18 18 :left :elide)
           (size 9 -1 :right)
           (mode 14 14 :left :elide)
           (read-only 4 4 :left)
           (modified 3 3 :left)
           (filename 40 0 :left :elide))))
  :bind (:map global-map ("C-x C-b" . ibuffer)))

(provide 'my-ibuffer)
;;; my-ibuffer.el ends here
