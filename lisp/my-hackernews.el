;;; my-hackernews.el --- Hacker News integration -*- lexical-binding: t; -*-

(require 'use-package)

(defvar my/hackernews-content-window nil
  "Reusable window for displaying Hacker News stories and comments.")

(defun my/hackernews--ensure-content-window ()
  "Return a reusable window for Hacker News content."
  (cond
   ((and (window-live-p my/hackernews-content-window)
         (eq (window-frame my/hackernews-content-window) (selected-frame))
         (not (eq my/hackernews-content-window (selected-window))))
    my/hackernews-content-window)
   ((one-window-p t)
    (setq my/hackernews-content-window (split-window-right)))
   (t
    (setq my/hackernews-content-window
          (or (window-in-direction 'right)
              (window-in-direction 'below)
              (next-window (selected-window) 'no-minibuf))))))

(defun my/hackernews-open-url (url)
  "Open URL in a reusable split window and keep focus on Hacker News."
  (interactive "sURL: ")
  (unless (and url (string-prefix-p "http" url))
    (user-error "No URL found at point"))
  (save-selected-window
    (select-window (my/hackernews--ensure-content-window))
    (eww-browse-url url)))

(defun my/hackernews-browse-url-action (button)
  "Open BUTTON's URL in the reusable Hacker News content window."
  (hackernews--visit button #'my/hackernews-open-url))

(defun my/hackernews-copy-url ()
  "Copy the URL of the Hacker News item at point."
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (if (and url (string-prefix-p "http" url))
        (progn
          (kill-new url)
          (message "Copied URL: %s" url))
      (message "No URL found at point."))))

(defun my/hackernews-open-comments ()
  "Jump to the comments link and open it in the Hacker News split window."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (search-backward "comments" (line-beginning-position) t)
        (let ((url (get-text-property (point) 'help-echo)))
          (if url
              (my/hackernews-open-url url)
            (message "No URL found for comments.")))
      (message "No comments link found on this line."))))

(use-package hackernews
  :ensure t
  :commands (hackernews)
  :bind (("C-c h n" . hackernews)
         :map hackernews-mode-map
         ("w" . my/hackernews-copy-url)
         ("c" . my/hackernews-open-comments))
  :config
  (setq hackernews-internal-browser-function #'my/hackernews-open-url)
  (button-type-put 'hackernews-link 'action #'my/hackernews-browse-url-action)
  (button-type-put 'hackernews-comment-count 'action #'my/hackernews-browse-url-action))

(provide 'my-hackernews)
;;; my-hackernews.el ends here
