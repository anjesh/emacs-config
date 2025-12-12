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

;; Neotree Configuration
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)) ;; Use icons if in GUI
  :bind (:map global-map
              ("C-x t t" . neotree-toggle)
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
  (vertico-mode))

;; Org Mode Configuration
(use-package org
  :ensure nil ;; Built-in
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
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
        '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a"))))

;; Backup Configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)     ;; Use version numbers for backups
(setq delete-old-versions t) ;; Silently delete excess backup versions
(setq kept-new-versions 6)   ;; Number of newest versions to keep
(setq kept-old-versions 2)   ;; Number of oldest versions to keep
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))