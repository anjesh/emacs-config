;;; my-dimmer.el --- Dimmer configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/toggle-window-highlighting ()
  "Toggle the dimmer mode for window highlighting."
  (interactive)
  (if (bound-and-true-p dimmer-mode)
      (progn
        (dimmer-mode -1)
        (message "Window Highlighting: DISABLED"))
    (dimmer-mode 1)
    (message "Window Highlighting: ENABLED")))

(use-package dimmer
  :ensure t
  :init
  (global-set-key (kbd "C-c w h") #'my/toggle-window-highlighting)
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-adjustment-mode :background)
  (setq dimmer-use-colors-space :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (custom-set-faces
   '(dimmer-dim-face ((t (:background "#e0e0e0")))))
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*Minibuf-[0-9]+\\*")
  (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*Echo Area[0-9]+\\*")
  (dimmer-mode t))

(provide 'my-dimmer)
;;; my-dimmer.el ends here
