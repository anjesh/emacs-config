;;; my-files.el --- File and persistence configuration -*- lexical-binding: t; -*-

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq auto-save-interval 50)
(setq auto-save-timeout 30)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))

;; Clipboard integration.
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; This connects the Emacs kill ring to the macOS pasteboard when running in the terminal.
(unless (display-graphic-p)
  (when (eq system-type 'darwin)
    (defun my-copy-to-clipboard (text &optional _push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (defun my-paste-from-clipboard ()
      (shell-command-to-string "pbpaste"))

    (setq interprogram-cut-function #'my-copy-to-clipboard)
    (setq interprogram-paste-function #'my-paste-from-clipboard)))

(provide 'my-files)
;;; my-files.el ends here
