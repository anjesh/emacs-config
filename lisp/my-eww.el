;;; my-eww.el --- EWW helpers and configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/eww-browse-at-point ()
  "Browse the URL at point using EWW."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (if url
        (eww url)
      (message "No URL at point."))))

(defun my/browse-url (url &optional _new-window)
  "Browse URL using EWW."
  (interactive (browse-url-interactive-arg "URL: "))
  (eww-browse-url url))

(use-package eww
  :ensure nil
  :commands (eww eww-browse-url)
  :init
  (global-set-key (kbd "C-c B") #'my/eww-browse-at-point)
  (global-set-key (kbd "C-c w e") #'eww)
  (global-set-key (kbd "C-c w b") #'my/browse-url)
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  (add-hook 'eww-after-render-hook
            (lambda ()
              (rename-buffer (format "*eww: %s*"
                                     (or (plist-get eww-data :title) "Web Page"))
                             t))))

(provide 'my-eww)
;;; my-eww.el ends here
