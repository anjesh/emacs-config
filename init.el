;; Initialize Package Manager and MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if missing
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --- Your Original Configuration ---

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)  ;; This enables sh, bash, zsh, etc.
   ))

;; Load custom settings from custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Load API keys from secrets.el
(load (locate-user-emacs-file "secrets.el") 'noerror)

;; --- End Original Configuration ---

;; Icons (Required for Neotree icons)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; Neotree Configuration
(use-package neotree
  :ensure t
  :hook (after-find-file . neotree-find) ;; Automatically show current file in Neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)) ;; Use icons if in GUI
  
  (defun neotree-toggle-dev ()
    "Toggle Neotree, always opening at ~/dev if showing."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (neotree-dir "~/dev")))

  :bind (:map global-map
              ("C-x t t" . neotree-toggle-dev)
              ("C-x t f" . neotree-find)))

;; Auto-Revert Mode (Auto-reload files changed on disk)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; macOS Option Key as Meta Fix for GUI Emacs
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; Ibuffer Configuration
(use-package ibuffer
  :ensure t
  :init
  (setq ibuffer-expert t) ;; More features in ibuffer
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

;; Projectile Configuration
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Vertico Configuration
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Enable vertico-directory extension for smart path deletion
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

;; Load vertico-directory (part of vertico)
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Org Mode Configuration
(use-package org
  :ensure nil ;; Built-in
  :hook (org-mode . (lambda () 
                      (visual-line-mode 1)  ;; Wrap lines at word boundary
                      (org-indent-mode 1))) ;; Cleaner indentation
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-directory "~/dev")
  ;; Recursively find .org files in ~/dev for agenda
  ;; Note: This can be slow if you have thousands of files.
  ;; Using 'directory-files-recursively' to build the list.
  (setq org-agenda-files (directory-files-recursively "~/dev" "\.org$"))
  
  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "WAITING(w@/!)" "FUTURE(f)" "|" "DONE(d!)" "CANCELLED(c@/!)")))

  ;; Default Capture File
  (setq org-default-notes-file (concat org-directory "/inbox.org"))

  ;; Capture Templates
  (setq org-capture-templates
        '( ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a"))))

;; Backup Configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)     ;; Use version numbers for backups
(setq delete-old-versions t) ;; Silently delete excess backup versions
(setq kept-new-versions 6)   ;; Number of newest versions to keep
(setq kept-old-versions 2)   ;; Number of oldest versions to keep
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode ("\.md\'" . markdown-mode)
  :hook (markdown-mode . visual-line-mode)) ;; Wrap lines at word boundary

;; Vterm Configuration (Requires cmake & libtool)
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key))
  :config
  (setq vterm-max-scrollback 10000))

;; Vterm Toggle (Pop up terminal)
(use-package vterm-toggle
  :ensure t
  :bind ("C-c t v" . vterm-toggle) ;; Changed from C-c t to C-c t v
  :config
  (setq vterm-toggle-fullscreen-p nil) ;; Open in split, not fullscreen
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p "vterm" (buffer-name buffer))))))
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.3))))

;; Enable mouse support in terminal (click, scroll, resize)
(xterm-mouse-mode 1)

;; Window Resizing
(global-set-key (kbd "C-c <") 'shrink-window-horizontally)
(global-set-key (kbd "C-c >") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c -") 'shrink-window) ; Vertical shrink
(global-set-key (kbd "C-c +") 'enlarge-window) ; Vertical enlarge
