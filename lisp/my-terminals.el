;;; my-terminals.el --- Terminal helpers and configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/current-path-for-terminal ()
  "Return a sensible directory for launching a terminal from the current context."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode) (dired-current-directory))
   ((derived-mode-p 'treemacs-mode)
    (let ((node-path (or (ignore-errors (treemacs--button-get (treemacs-node-at-point) :path))
                         (ignore-errors (treemacs-button-get (treemacs-node-at-point) :path))
                         (ignore-errors
                           (treemacs-copy-path-at-point)
                           (substring-no-properties (current-kill 0))))))
      (when (and node-path (file-exists-p node-path))
        (if (file-directory-p node-path)
            node-path
          (file-name-directory node-path)))))
   (t (if buffer-file-name
          (file-name-directory buffer-file-name)
        default-directory))))

(defun my/open-ghostty-here ()
  "Open Ghostty in the current buffer's directory, dired dir, or Treemacs node."
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (progn
          (message "Opening Ghostty in: %s" path)
          (start-process "open-ghostty" nil "open" "-a" "Ghostty" path))
      (message "Could not determine directory."))))

(defun my/open-vterm-here ()
  "Open vterm in the current buffer's directory, dired dir, or Treemacs node."
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (vterm t))
      (message "Could not determine directory."))))

(defun my/open-vterm-here-side ()
  "Open a new vterm in a right side window at the current location."
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (require 'vterm)
          (let* ((buf (vterm--internal #'ignore t))
                 (win (display-buffer-in-side-window
                       buf
                       '((side . right)
                         (window-width . 0.3)))))
            (select-window win)))
      (message "Could not determine directory."))))

(defun my/open-eshell-here ()
  "Open eshell in the current buffer's directory, dired dir, or Treemacs node."
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (eshell t))
      (message "Could not determine directory."))))

(global-set-key (kbd "C-c t g") #'my/open-ghostty-here)
(global-set-key (kbd "C-c t v") #'my/open-vterm-here)
(global-set-key (kbd "C-c t e") #'my/open-eshell-here)

(use-package ghostel
  :ensure t)

(use-package vterm
  :ensure t
  :commands (vterm vterm--internal)
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key))
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines t)))
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :bind ("C-c t V" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p "vterm" (buffer-name buffer))))))
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4))))

(provide 'my-terminals)
;;; my-terminals.el ends here
