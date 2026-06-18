;;; my-gui.el --- GUI, theme, and font configuration -*- lexical-binding: t; -*-

(require 'use-package)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(defun my/set-gui-font ()
  "Set the font family and size for GUI Emacs."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Iosevka" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 140)
    (set-face-attribute 'variable-pitch nil :family "Iosevka" :height 140)))

(my/set-gui-font)
(add-hook 'window-setup-hook #'my/set-gui-font)
(add-hook 'server-after-make-frame-hook #'my/set-gui-font)

;; macOS Option Key as Meta Fix for GUI Emacs
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

(provide 'my-gui)
;;; my-gui.el ends here
