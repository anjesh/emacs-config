;;; my-mouse.el --- Mouse and wheel configuration -*- lexical-binding: t; -*-

;; Enable mouse support in terminal (click, scroll, resize)
(xterm-mouse-mode 1)

;; Disable mouse wheel text scaling by default.
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "M-<wheel-up>"))
(global-unset-key (kbd "M-<wheel-down>"))

(defun my/toggle-mouse-wheel-zoom ()
  "Toggle mouse wheel zooming (text scaling)."
  (interactive)
  (if (lookup-key global-map (kbd "C-<wheel-up>"))
      (progn
        (global-unset-key (kbd "C-<wheel-up>"))
        (global-unset-key (kbd "C-<wheel-down>"))
        (global-unset-key (kbd "M-<wheel-up>"))
        (global-unset-key (kbd "M-<wheel-down>"))
        (message "Mouse wheel zoom: DISABLED"))
    (global-set-key (kbd "C-<wheel-up>") #'text-scale-increase)
    (global-set-key (kbd "C-<wheel-down>") #'text-scale-decrease)
    (global-set-key (kbd "M-<wheel-up>") #'text-scale-increase)
    (global-set-key (kbd "M-<wheel-down>") #'text-scale-decrease)
    (message "Mouse wheel zoom: ENABLED")))

(global-set-key (kbd "C-c z") #'my/toggle-mouse-wheel-zoom)

(provide 'my-mouse)
;;; my-mouse.el ends here
