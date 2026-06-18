;;; my-spray.el --- Spray configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package spray
  :ensure t
  :bind ("<f6>" . spray-mode)
  :config
  (setq spray-wpm 420
        spray-save-point t))

(provide 'my-spray)
;;; my-spray.el ends here
