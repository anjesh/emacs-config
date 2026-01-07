
;;; init.el --- User configuration file  -*- lexical-binding: t; -*-

;; Enable Syntax Highlighting Early
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Initialize Package Manager and MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Clean up UI
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Enable visual line movement globally
(setq-default line-move-visual t)
(global-visual-line-mode t)

;; --- Spell Checking (Flyspell) ---
;; Requires: brew install hunspell
(use-package flyspell
  :ensure nil ;; Built-in
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (("C-c s C" . my/toggle-spell-check)
         ("C-c s c" . flyspell-correct-word-before-point))
  :config
  (cond
   ;; Prefer Hunspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
   ;; Fallback to Aspell
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))))

  (defun my/toggle-spell-check ()
    "Toggle flyspell-mode on/off."
    (interactive)
    (if (bound-and-true-p flyspell-mode)
        (progn
          (flyspell-mode -1)
          (message "Spell check: OFF"))
      (flyspell-mode 1)
      (message "Spell check: ON"))))

;; Syntax Highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#f2f2f2") ; Very light gray

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

;; Font Configuration (GUI)
(defun my/set-gui-font ()
  "Set the font family and size for GUI Emacs."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Iosevka" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 140)
    (set-face-attribute 'variable-pitch nil :family "Iosevka" :height 140)))

;; Apply font settings at startup and when creating new frames
(my/set-gui-font)
(add-hook 'window-setup-hook 'my/set-gui-font)
(add-hook 'server-after-make-frame-hook 'my/set-gui-font)

;; Undo Tree (Visual Undo History)
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  ;; Persist undo history to a file
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :bind
  ("C-x u" . undo-tree-visualize))

;; Install use-package if missing
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure PATH is inherited from shell (Critical for macOS)
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

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

;; Treemacs Configuration
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-render-file                   nil
          treemacs-root-consolidation            'always
          treemacs-show-cursor                   nil
          treemacs-show-expand-min               t
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-case-insensitive-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-width-is-initially-locked     nil
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Auto-Revert Mode (Auto-reload files changed on disk)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Auto-save Settings
(setq auto-save-interval 50)
(setq auto-save-timeout 30)

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

;; Orderless: Fuzzy matching for completion
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (optional)
  ;; (setq orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Vertico Configuration
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Enable vertico-directory extension for smart path deletion
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

;; Embark: Actions for the item at point (buffer, file, etc.)
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; Standard key for actions
   ("M-k" . embark-act)         ;; Bind M-k to actions as well (e.g. M-k k to kill)
   ("C-;". embark-dwim)        ;; Do What I Mean
   ("C-h B" . embark-bindings)) ;; Help
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("`\*Embark Collect \(Live\|Completions\)\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Load vertico-directory (part of vertico)
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Consult: Richer search and navigation
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)       ;; Supercharged buffer switcher
         ("C-s"   . consult-line)         ;; Search within file (visual)
         ("M-y"   . consult-yank-pop)     ;; Paste from clipboard history
         ("M-g g" . consult-goto-line)))  ;; Go to line with preview

;; Marginalia: Annotations (file size, mode, etc.) in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; --- Uniform Font Style for Headers ---
(defun my/reset-header-styles ()
  "Force headers to use the default font family and size."
  (interactive)
  (let ((font-family (face-attribute 'default :family)))
    ;; Org Mode Headers
    (with-eval-after-load 'org
      ;; Only bold the highest level (level 1)
      (set-face-attribute 'org-level-1 nil :family font-family :height 1.0 :weight 'bold)
      ;; Set others to normal weight
      (dolist (face '(org-level-2 org-level-3 org-level-4
                      org-level-5 org-level-6 org-level-7 org-level-8))
        (set-face-attribute face nil :family font-family :height 1.0 :weight 'normal)))
    
    ;; Markdown Mode Headers
    (with-eval-after-load 'markdown-mode
      (dolist (face '(markdown-header-face-1 markdown-header-face-2
                      markdown-header-face-3 markdown-header-face-4
                      markdown-header-face-5 markdown-header-face-6))
        (set-face-attribute face nil :family font-family :height 1.0 :weight 'bold)))))

;; Apply on startup and after theme load
(add-hook 'enable-theme-functions (lambda (&rest _) (my/reset-header-styles)))
;; Run once for currently loaded modes
(my/reset-header-styles)

;; Org Mode Configuration
(use-package org
  :ensure nil ;; Built-in
  :hook (org-mode . (lambda () 
                      (font-lock-mode 1)
                      (font-lock-ensure)
                      (visual-line-mode 1)  ;; Wrap lines at word boundary
                      (org-indent-mode 1))) ;; Cleaner indentation
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-c b" . my/toggle-org-bullets)) ;; Toggle bullets
  :config
  (setq org-directory "~/dev")
  (setq org-hide-leading-stars nil) ;; Show stars by default
  (setq browse-url-browser-function 'eww-browse-url) ;; Open links in EWW by default
  
  ;; Use org-bullets to replace the last star with a space
  (use-package org-bullets
    :ensure t
    :config
    (setq org-bullets-bullet-list '(" "))) ;; Use a space as the bullet

  (defun my/toggle-org-bullets ()
    "Toggle between hidden bullets (spaces) and visible stars."
    (interactive)
    (if (bound-and-true-p org-bullets-mode)
        (progn
          (org-bullets-mode -1)
          (setq org-hide-leading-stars nil)
          (font-lock-flush)
          (message "Org Bullets: Visible (Stars)"))
      (org-bullets-mode 1)
      (setq org-hide-leading-stars t)
      (font-lock-flush)
      (message "Org Bullets: Hidden (Spaces)")))

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-c b" . my/toggle-org-bullets)) ;; Toggle bullets
  :config
  (setq org-directory "~/dev")
  (setq org-export-with-section-numbers nil) ;; Disable numbered headings globally
  (setq org-hide-leading-stars t) ;; Hide all but the last star
  
  ;; Find .org files recursively but exclude journal, obsidian-notes, client-projects, and slack
  (setq org-agenda-files 
        (seq-filter 
         (lambda (file)
           (not (string-match-p "/\\(journal\\|obsidian-notes\\|client-projects\\|slack\\)/" file)))
         (directory-files-recursively "~/dev" "\\.org$")))
  
  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "WAITING(w@/!)" "FUTURE(f)" "|" "DONE(d@)" "CANCELLED(c@/!)")))

  ;; Custom TODO keyword faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DOING" :foreground "orange" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("FUTURE" :foreground "gray")
          ("DONE" :foreground "green" :weight bold)
          ("CANCELLED" :foreground "light gray")))

  ;; Show parent headings (breadcrumbs) in Agenda
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c %b%?-12t% s")
          (todo   . " %i %-12:c %b")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; Default Capture File
  (setq org-default-notes-file (concat org-directory "/inbox.org"))

  ;; Capture Templates
  (setq org-capture-templates
        '( ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
           ("w" "Work Task" entry (file+headline "~/dev/orgfiles/work.org" "Work Tasks")
           "* TODO %?\n  %i\n  %a")
           ("p" "Personal Task" entry (file+headline "~/dev/orgfiles/personal.org" "Personal Tasks")
           "* TODO %?\n  %i\n  %a"))))

;; Backup Configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)     ;; Use version numbers for backups
(setq delete-old-versions t) ;; Silently delete excess backup versions
(setq kept-new-versions 6)   ;; Number of newest versions to keep
(setq kept-old-versions 2)   ;; Number of oldest versions to keep
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))

;; Adaptive Wrap (Visual indentation for wrapped lines)
(use-package adaptive-wrap
  :ensure t
  :defer t)

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  :hook (markdown-mode . (lambda () 
                           (font-lock-mode 1)
                           (font-lock-ensure) ;; Force refontification
                           (visual-line-mode 1)))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  ;; Ensure tables use the default monospace font for alignment
  (with-eval-after-load 'markdown-mode
    (set-face-attribute 'markdown-table-face nil :family (face-attribute 'default :family))))

(use-package markdown-preview-mode
  :ensure t
  :defer t
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css")))

;; CSV Mode
(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . (lambda ()
                      (csv-align-mode 1)
                      (csv-header-line-mode 1))))

;; Iedit - Edit all occurrences of a symbol/region simultaneously
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; PDF Tools (GUI only)
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width) ;; or 'fit-page
  (setq pdf-view-continuous t) ;; Continuous scroll
  (add-hook 'pdf-view-mode-hook (lambda () 
                                  (display-line-numbers-mode -1) ;; Disable line numbers in PDF
                                  (pdf-isearch-minor-mode)
                                  (auto-revert-mode 1))))

;; Helper to open files in external app (macOS Preview, etc.)
(defun my/open-in-external-app ()
  "Open the current file, dired-marked file, or treemacs node in external app."
  (interactive)
  (let ((file (cond
               ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
               ((derived-mode-p 'treemacs-mode)
                ;; Try multiple ways to get the path (robustness for different treemacs versions)
                (or (ignore-errors (treemacs--button-get (treemacs-node-at-point) :path))
                    (ignore-errors (treemacs-button-get (treemacs-node-at-point) :path))
                    (ignore-errors
                      (treemacs-copy-path-at-point)
                      (substring-no-properties (current-kill 0)))))
               (t buffer-file-name))))
    (if (and file (not (string-empty-p file)))
        (progn
          (start-process "open-external" nil "open" file)
          (message "Opened in external app: %s" file))
      (message "Could not determine file path."))))

(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c o") 'my/open-in-external-app)

;; Helper to open Ghostty terminal at current location
(defun my/open-ghostty-here ()
  "Open Ghostty terminal in the current buffer's directory, dired dir, or treemacs node."
  (interactive)
  (let ((path (cond
               ((derived-mode-p 'dired-mode) (dired-current-directory))
               ((derived-mode-p 'treemacs-mode)
                (let ((node-path (or (ignore-errors (treemacs--button-get (treemacs-node-at-point) :path))
                                     (ignore-errors (treemacs-button-get (treemacs-node-at-point) :path))
                                     (ignore-errors
                                       (treemacs-copy-path-at-point)
                                       (substring-no-properties (current-kill 0))))))
                  (if (and node-path (file-exists-p node-path))
                      (if (file-directory-p node-path)
                          node-path
                        (file-name-directory node-path))
                    nil)))
               (t (if buffer-file-name
                      (file-name-directory buffer-file-name)
                    default-directory)))))
    (if (and path (not (string-empty-p path)))
        (progn
          (message "Opening Ghostty in: %s" path)
          (start-process "open-ghostty" nil "open" "-a" "Ghostty" path))
      (message "Could not determine directory."))))

(defun my/open-vterm-here ()
  "Open vterm in the current buffer's directory, dired dir, or treemacs node."
  (interactive)
  (let ((path (cond
               ((derived-mode-p 'dired-mode) (dired-current-directory))
               ((derived-mode-p 'treemacs-mode)
                (let ((node-path (or (ignore-errors (treemacs--button-get (treemacs-node-at-point) :path))
                                     (ignore-errors (treemacs-button-get (treemacs-node-at-point) :path))
                                     (ignore-errors
                                       (treemacs-copy-path-at-point)
                                       (substring-no-properties (current-kill 0))))))
                  (if (and node-path (file-exists-p node-path))
                      (if (file-directory-p node-path)
                          node-path
                        (file-name-directory node-path))
                    nil)))
               (t (if buffer-file-name
                      (file-name-directory buffer-file-name)
                    default-directory)))))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (vterm t))
      (message "Could not determine directory."))))

(global-set-key (kbd "C-c t g") 'my/open-ghostty-here)
(global-set-key (kbd "C-c t t") 'my/open-vterm-here)

;; Bind 'O' in Treemacs to open externally, 'T' to open Ghostty, and 'C' to copy path
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "O") #'my/open-in-external-app)
  (define-key treemacs-mode-map (kbd "T") #'my/open-ghostty-here)
  (define-key treemacs-mode-map (kbd "v") #'my/open-vterm-here)
  (define-key treemacs-mode-map (kbd "C") #'my/treemacs-copy-path-to-clipboard)
  (define-key treemacs-mode-map (kbd "L") #'org-store-link)

  ;; Disable Evil in Treemacs (use Emacs state)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'treemacs-mode 'emacs)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "RET") #'treemacs-RET-action)
    (evil-define-key 'normal treemacs-mode-map (kbd "RET") #'treemacs-RET-action)))

;; Helper to copy the full path of file/directory
(defun my/treemacs-copy-path-to-clipboard ()
  "Copy the full path of the current file, dired-marked file, or treemacs node to clipboard."
  (interactive)
  (let ((path (cond
               ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
               ((derived-mode-p 'treemacs-mode)
                ;; Try multiple ways to get the path (robustness for different treemacs versions)
                (or (ignore-errors (treemacs--button-get (treemacs-node-at-point) :path))
                    (ignore-errors (treemacs-button-get (treemacs-node-at-point) :path))
                    (ignore-errors
                      (treemacs-copy-path-at-point)
                      (substring-no-properties (current-kill 0)))))
               (t buffer-file-name))))
    (if (and path (not (string-empty-p path)))
        (progn
          (kill-new path)
          (message "Copied path to clipboard: %s" path))
      (message "Could not determine path to copy."))))

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

;; Mistty Configuration (Alternative to Vterm with better shell integration)
(use-package mistty
  :ensure t
  :bind ("C-c t m" . mistty))

;; Popup package (dependency for gemini-cli.el)
(use-package popup :ensure t)

;; --- Mouse Configuration ---
;; Enable mouse support in terminal (click, scroll, resize)
(xterm-mouse-mode 1)

;; Disable mouse wheel text scaling by default
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
    (global-set-key (kbd "C-<wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "C-<wheel-down>") 'text-scale-decrease)
    (global-set-key (kbd "M-<wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "M-<wheel-down>") 'text-scale-decrease)
    (message "Mouse wheel zoom: ENABLED")))

(global-set-key (kbd "C-c z") 'my/toggle-mouse-wheel-zoom)

;; Clipboard Integration (Ensure Emacs copies to system clipboard)
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; macOS Terminal Clipboard Support (pbcopy/pbpaste)
;; This connects the Emacs kill ring to the macOS pasteboard when running in the terminal.
(unless (display-graphic-p)
  (when (eq system-type 'darwin)
    (defun my-copy-to-clipboard (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (defun my-paste-from-clipboard ()
      (shell-command-to-string "pbpaste"))

    (setq interprogram-cut-function 'my-copy-to-clipboard)
    (setq interprogram-paste-function 'my-paste-from-clipboard)))

;; Window Management
(global-set-key (kbd "C-c <") 'shrink-window-horizontally)
(global-set-key (kbd "C-c >") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c -") 'shrink-window) ; Vertical shrink
(global-set-key (kbd "C-c +") 'enlarge-window) ; Vertical enlarge

;; macOS-style Beginning/End of Buffer (Reliable Alternatives)
(global-set-key (kbd "C-c u") 'beginning-of-buffer)    ;; Up to Top
(global-set-key (kbd "C-c d") 'end-of-buffer)          ;; Down to Bottom

;; Helper to open readme.md
(global-set-key (kbd "C-c r") (lambda () (interactive) (find-file (expand-file-name "readme.md" user-emacs-directory))))

;; Org Link Support
(global-set-key (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c C-l") 'org-insert-link)

;; --- Evil Mode (Vim Keybindings) ---
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil) ;; Do not override any existing keybindings
  (setq evil-respect-visual-line-mode t) ;; Respect visual line movement
  ;; (evil-mode 1) ;; Disabled by default
  :config
  ;; Force 'j' and 'k' to move by visual lines, not logical lines
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line))

(defun my/toggle-evil-mode ()
  "Toggle Evil mode on/off."
  (interactive)
  (if (bound-and-true-p evil-mode)
      (progn
        (evil-mode -1)
        (message "Evil Mode DISABLED (Emacs Standard)"))
    (evil-mode 1)
    (message "Evil Mode ENABLED (Vim Bindings)")))

(global-set-key (kbd "C-c v") 'my/toggle-evil-mode)

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

;; --- Org Mode Keybinding Fixes (Evil Compatibility) ---
(with-eval-after-load 'org
  ;; Force standard Emacs bindings in Org Mode map
  (define-key org-mode-map (kbd "TAB") 'org-cycle)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "M-<left>") 'org-metaleft)
  (define-key org-mode-map (kbd "M-<right>") 'org-metaright)
  
  ;; Reliable C-c Arrow key alternatives
  (define-key org-mode-map (kbd "C-c <left>") 'org-metaleft)
  (define-key org-mode-map (kbd "C-c <right>") 'org-metaright)
  (define-key org-mode-map (kbd "C-c <up>") 'org-metaup)
  (define-key org-mode-map (kbd "C-c <down>") 'org-metadown))

(with-eval-after-load 'evil
  ;; Force Evil Normal state to respect these Org bindings
  (let ((map org-mode-map))
    (evil-define-key 'normal map (kbd "TAB") 'org-cycle)
    (evil-define-key 'normal map (kbd "<tab>") 'org-cycle)
    (evil-define-key 'normal map (kbd "M-<left>") 'org-metaleft)
    (evil-define-key 'normal map (kbd "M-<right>") 'org-metaright)
    (evil-define-key 'normal map (kbd "C-c <left>") 'org-metaleft)
    (evil-define-key 'normal map (kbd "C-c <right>") 'org-metaright)
    (evil-define-key 'normal map (kbd "C-c <up>") 'org-metaup)
    (evil-define-key 'normal map (kbd "C-c <down>") 'org-metadown)))

;; --- Journal Navigation ---

(defun my/open-daily-journal ()
  "Open today's daily journal file."
  (interactive)
  (let ((daily-path (format-time-string "~/dev/journal/daily/%Y/%m/%Y-%m-%d.md")))
    (if (file-exists-p daily-path)
        (find-file daily-path)
      (if (y-or-n-p (format "Daily journal not found at %s. Create it? " daily-path))
          (progn
            (make-directory (file-name-directory daily-path) t)
            (find-file daily-path))
        (message "Cancelled.")))))

(defun my/open-weekly-journal ()
  "Open this week's journal file. If today is Sunday, opens next week's file."
  (interactive)
  (let* ((now (decode-time))
         (dow (nth 6 now)) ;; 0 = Sunday
         ;; If Sunday, use time + 1 day to get into next ISO week
         (target-time (if (= dow 0)
                          (time-add (current-time) (* 24 3600))
                        (current-time)))
         (weekly-path (format-time-string "~/dev/journal/weekly/%Y/week-%V.md" target-time)))
    (if (file-exists-p weekly-path)
        (find-file weekly-path)
      (if (y-or-n-p (format "Weekly journal not found at %s. Create it? " weekly-path))
          (progn
             (make-directory (file-name-directory weekly-path) t)
             (find-file weekly-path))
        (message "Cancelled.")))))

(defun my/open-monthly-journal ()
  "Open this month's journal file."
  (interactive)
  (let ((monthly-path (format-time-string "~/dev/journal/monthly/%Y/%m-%B.org")))
    (if (file-exists-p monthly-path)
        (find-file monthly-path)
      (if (y-or-n-p (format "Monthly journal not found at %s. Create it? " monthly-path))
          (progn
             (make-directory (file-name-directory monthly-path) t)
             (find-file monthly-path)
             (insert "#+TITLE: Monthly Journal - " (format-time-string "%B %Y") "\n#+STARTUP: showall\n\n"))
        (message "Cancelled.")))))

(defun my/insert-daily-journal-entry ()
  "Insert a daily journal template with Review and Time blocks."
  (interactive)
  (let ((date-string (format-time-string "%Y-%m-%d %a")))
    (insert "* " date-string "\n")
    (insert "** Review\n")
    (insert "*** Win\n")
    (insert "*** Gratitude\n")
    (insert "*** Anxiety\n")
    (insert "*** Kindness\n")
    (insert "*** Interesting\n")
    (insert "*** Ishaan\n")
    (insert "*** Deepika\n")
    (insert "** Intermittent\n")
    (insert "*** " (format-time-string "%H:%M") "\n")))

(global-set-key (kbd "C-c j d") 'my/open-daily-journal)
(global-set-key (kbd "C-c j w") 'my/open-weekly-journal)
(global-set-key (kbd "C-c j m") 'my/open-monthly-journal)
(global-set-key (kbd "C-c j n") 'my/insert-daily-journal-entry) ;; New Entry Shortcut
;; --- Ebook Reading Configuration ---

;; nov.el - EPUB Reader
(use-package nov
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\.epub\'" . nov-mode))
    (add-to-list 'auto-mode-alist '("\.EPUB\'" . nov-mode))
    (setq nov-text-width 80) ;; comfortable reading width
    (add-hook 'nov-mode-hook 'visual-line-mode)
    (with-eval-after-load 'evil
      (evil-set-initial-state 'nov-mode 'emacs))
    
    ;; Enable images (works in GUI Emacs)
    (require 'shr)
    (setq shr-inhibit-images nil)
    (setq shr-use-fonts t)
    (setq shr-max-image-proportion 0.8)

    ;; Custom image handling: small images, clickable to enlarge
    (defun my-nov-view-image (path)
      "View the image at PATH in a new buffer."
      (interactive)
      (let ((buf (find-file-noselect path)))
        (with-current-buffer buf
          (image-mode)
          (pop-to-buffer buf))))

    (defun my-nov-insert-image (path alt)
      "Insert an image for PATH at point, falling back to ALT.
Images are resized to a smaller dimension (30% of window) and are clickable."
      (let ((type (if (or (and (fboundp 'image-transforms-p) (image-transforms-p))
                          (not (fboundp 'imagemagick-types)))
                      nil
                    'imagemagick)))
        (if (not (display-graphic-p))
            (insert alt)
          (seq-let (x1 y1 x2 y2) (window-inside-pixel-edges
                                  (get-buffer-window (current-buffer)))
            (let* ((max-width (truncate (* 0.3 (- x2 x1))))
                   (max-height (truncate (* 0.3 (- y2 y1))))
                   (image
                    (ignore-errors
                      (create-image path type nil
                                    :ascent 100
                                    :max-width max-width
                                    :max-height max-height))))
              (if image
                  (let ((map (make-sparse-keymap)))
                    (define-key map [mouse-1] (lambda () (interactive) (my-nov-view-image path)))
                    (define-key map (kbd "RET") (lambda () (interactive) (my-nov-view-image path)))
                    (insert (propertize " "
                                        'display image
                                        'keymap map
                                        'help-echo "Click to enlarge")))
                (insert alt)))))))

    (advice-add 'nov-insert-image :override #'my-nov-insert-image)))
;; calibredb - Interface for Calibre
(use-package calibredb
  :ensure t
  :config
  ;; REQUIRED: Path to your Calibre Library.
  (setq calibredb-root-dir "~/Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Calibre/")))
  
  ;; Force calibredb to use Emacs state (standard keys) instead of Evil (Vim)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'calibredb-search-mode 'emacs)
    (evil-set-initial-state 'calibredb-show-mode 'emacs))

  ;; Custom function to force open in nov.el
  (defun my/calibredb-open-with-nov ()
    "Open the current book with nov.el."
    (interactive)
    (let ((file (calibredb-get-file-path (car (calibredb-find-candidate-at-point)) t)))
      (if file
          (progn
            (find-file file)
            (when (string-suffix-p "epub" file t)
              (nov-mode)))
        (message "No file found."))))
  
  :bind
  ("C-c e" . calibredb)
  (:map calibredb-search-mode-map
        ("RET" . my/calibredb-open-with-nov))
  (:map calibredb-show-mode-map
        ("RET" . my/calibredb-open-with-nov))) ;; Bind C-c e to open CalibreDB

;; --- Window Highlighting (Dimmer) ---
(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.5) ;; Strong 50% dimming
  (setq dimmer-adjustment-mode :foreground) 
  (setq dimmer-use-colors-space :rgb)
  
  ;; IMPORTANT: Ensure it works on splits within the same frame
  (setq dimmer-watch-frame-focus-events nil) ;; Don't dim everything when switching apps
  
  ;; Explicitly set the dimmed face to a very light gray for high contrast
  (custom-set-faces
   '(dimmer-dim-face ((t (:foreground "#bcc5c5")))))
  
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  
  ;; Exclude minibuffer and echo area
  (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*Minibuf-[0-9]+\\*")
  (add-to-list 'dimmer-buffer-exclusion-regexps "^ \\*Echo Area[0-9]+\\*")

  (dimmer-mode t))

(defun my/toggle-window-highlighting ()
  "Toggle the dimmer mode for window highlighting."
  (interactive)
  (if (bound-and-true-p dimmer-mode)
      (progn
        (dimmer-mode -1)
        (message "Window Highlighting: DISABLED"))
    (dimmer-mode 1)
    (message "Window Highlighting: ENABLED")))

(global-set-key (kbd "C-c w h") 'my/toggle-window-highlighting)

;; --- Web Browsing (EWW) ---

;; EWW (Text-based, built-in)
(use-package eww
  :ensure nil
  :config
  (setq eww-search-prefix "https://google.com/search?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; Rename buffer to page title
  (add-hook 'eww-after-render-hook
            (lambda ()
              (rename-buffer (format "*eww: %s*"
                                     (or (plist-get eww-data :title) "Web Page"))
                             t))))

(defun my/browse-url (url &optional new-window)
  "Browse URL using EWW."
  (interactive (browse-url-interactive-arg "URL: "))
  (eww-browse-url url))

(global-set-key (kbd "C-c w e") 'eww)
(global-set-key (kbd "C-c w b") 'my/browse-url)

;; --- Dictionary (Minibuffer) ---
;; (Dictionary configuration removed)

;; --- YAML Support ---
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package hackernews
  :ensure t
  :config
  (setq hackernews-default-browser 'eww-browse-url) ;; Open links in EWW (inside Emacs)
  
  (defun my/hackernews-copy-url ()
    "Copy the URL of the Hacker News item at point."
    (interactive)
    (let ((url (get-text-property (point) 'help-echo))) ;; hackernews puts URL in help-echo
      (if (and url (string-prefix-p "http" url))
          (progn
            (kill-new url)
            (message "Copied URL: %s" url))
        (message "No URL found at point."))))
  
  (defun my/hackernews-open-comments ()
    "Jump to the 'comments' button and open it in EWW."
    (interactive)
    (save-excursion
      (end-of-line)
      (if (search-backward "comments" (line-beginning-position) t)
          (let ((url (get-text-property (point) 'help-echo)))
            (if url
                (eww-browse-url url)
              (message "No URL found for comments.")))
        (message "No comments link found on this line."))))

  :bind
  ("C-c h n" . hackernews)
  (:map hackernews-mode-map
        ("w" . my/hackernews-copy-url)
        ("c" . my/hackernews-open-comments)))

;; --- Gemini CLI Integration ---
(use-package eat
  :ensure t
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (setq truncate-lines t))))

(use-package gemini-cli
  :ensure t
  :vc (:url "https://github.com/linchen2chris/gemini-cli.el" :rev :newest)
  :config
  (setq gemini-cli-terminal-backend 'vterm)
  (setq gemini-cli-optimize-window-resize nil)
  (setq gemini-cli-program "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/gemini")
  (gemini-cli-mode)
  (add-hook 'gemini-cli-start-hook
            (lambda ()
              (evil-emacs-state)
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (setq truncate-lines t)
              (local-set-key (kbd "M-w") 'kill-ring-save)))
  :bind
  (("C-c g g" . gemini-cli)                  ;; Start Gemini
   ("C-c g s" . gemini-cli-send-command)     ;; Send command from minibuffer
   ("C-c g r" . gemini-cli-send-region)      ;; Send selected region
   ("C-c g o" . gemini-cli-send-buffer-file) ;; Send current file
   ("C-c g t" . gemini-cli-toggle)           ;; Toggle Gemini window
   ("C-c g d" . gemini-cli-start-in-directory)
   ("C-c g q" . gemini-cli-kill)))

;; --- Agent Shell Integration ---
;; Ensure agent-shell is installed
(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-agent-configs
        '((:name "Gemini" 
           :command "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/gemini" 
           :args ("chat"))))
  (global-set-key (kbd "C-c A") 'agent-shell))

;; (require 'agent-shell) ;; Removed to prevent startup error if missing


;; --- Claude Code CLI Integration ---

;; Inheritenv (dependency for claude-code)
(use-package inheritenv
  :ensure t
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (setq claude-code-terminal-backend 'vterm)
  (setq claude-code-optimize-window-resize nil)
  (setq claude-code-program "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/claude")
  (claude-code-mode)
  (add-hook 'claude-code-start-hook
            (lambda ()
              (evil-emacs-state)
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (setq truncate-lines t)
              (local-set-key (kbd "M-w") 'kill-ring-save)))
  :bind
  (("C-c C c" . claude-code)                  ;; Start Claude
   ("C-c C s" . claude-code-send-command)     ;; Send command
   ("C-c C r" . claude-code-send-region)      ;; Send region
   ("C-c C o" . claude-code-send-buffer-file) ;; Send file
   ("C-c C t" . claude-code-toggle)           ;; Toggle window
   ("C-c C d" . claude-code-start-in-directory)
   ("C-c C q" . claude-code-kill)))

;; --- Qwen CLI Integration ---
(use-package qwen-cli
  :ensure t
  :load-path "elpa/qwen-cli" ;; Specify load-path since it's a local package
  :config
  (setq qwen-cli-terminal-backend 'vterm)
  (setq qwen-cli-optimize-window-resize nil)
  (setq qwen-cli-program "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/qwen")
  (qwen-cli-mode)
  (add-hook 'qwen-cli-start-hook
            (lambda ()
              (evil-emacs-state)
              (display-line-numbers-mode -1)
              (visual-line-mode -1)
              (setq truncate-lines t)
              (local-set-key (kbd "M-w") 'kill-ring-save)))
  :bind
  (("C-c Q Q" . qwen-cli)                     ;; Start Qwen
   ("C-c Q s" . qwen-cli-send-command)        ;; Send command from minibuffer
   ("C-c Q r" . qwen-cli-send-region)         ;; Send selected region
   ("C-c Q o" . qwen-cli-send-buffer-file)    ;; Send current file
   ("C-c Q t" . qwen-cli-toggle)              ;; Toggle Qwen window
   ("C-c Q d" . qwen-cli-start-in-directory)
   ("C-c Q q" . qwen-cli-kill)))

;; --- Slack Integration ---
(use-package alert
  :ensure t
  :commands (alert)
  :config
  ;; Use 'message' (minibuffer) by default. 
  ;; For macOS system notifications, install terminal-notifier (brew install terminal-notifier)
  ;; and change this to 'notifier.
  (setq alert-default-style 'message))

(use-package slack
  :ensure t
  :vc (:url "https://github.com/yuya373/emacs-slack" :rev :newest)
  :commands (slack-start)
  :bind (("C-c s s" . slack-start)
         ("C-c s m" . slack-message-embed-mention)
         ("C-c s q" . slack-ws-close)
         ("C-c s r" . slack-select-rooms)  ;; List all channels/DMs with unread status
         ("C-c s u" . slack-select-unread-rooms) ;; List ONLY unread channels/DMs
         ("C-c s t" . slack-all-threads))  ;; View threaded conversations
  :init
  (setq slack-buffer-function #'switch-to-buffer) ; How to open slack buffers
  (setq slack-prefer-current-team t)
  :config
  (setq slack-enable-notification t) ;; Enable notifications via 'alert'
  
  (slack-register-team
   :name "younginnovations"
   :default t
   :token (auth-source-pick-first-password
           :host "younginnovations.slack.com"
           :user "anjesh@yipl.com.np")
   :cookie (auth-source-pick-first-password
            :host "younginnovations.slack.com"
            :user "anjesh@yipl.com.np^cookie")))

;; --- Slack Logging & Linking ---
(with-eval-after-load 'org
  (org-link-set-parameters "slack"
                           :follow #'my/slack-org-link-follow))

(defun my/slack-org-link-follow (path)
  "Follow a Slack link of the form team-id:room-id or team-id:room-id:thread-ts."
  (let* ((parts (split-string path ":"))
         (team-id (nth 0 parts))
         (room-id (nth 1 parts))
         (thread-ts (nth 2 parts))
         ;; Fix: Use (hash-table-values slack-teams-by-token) instead of non-existent slack-teams
         (team (cl-find team-id (hash-table-values slack-teams-by-token) :key (lambda (t) (oref t id)) :test #'string=))
         (room (when team
                 (or (gethash room-id (oref team channels))
                     (gethash room-id (oref team groups))
                     (gethash room-id (oref team ims))))))
    (if (and team room)
        (if thread-ts
            (slack-open-message team room thread-ts thread-ts)
          (slack-room-display room team))
      (message "Slack team or room not found"))))

(defun my/slack-user-org-link-follow (path)
  "Follow a Slack user link of the form team-id:user-id."
  (let* ((parts (split-string path ":"))
         (team-id (nth 0 parts))
         (user-id (nth 1 parts))
         (team (cl-find team-id (hash-table-values slack-teams-by-token) :key (lambda (t) (oref t id)) :test #'string=)))
    (if team
        (slack-buffer-display (slack-create-user-profile-buffer team user-id))
      (message "Slack team not found"))))

(with-eval-after-load 'org
  (org-link-set-parameters "slack" :follow #'my/slack-org-link-follow)
  (org-link-set-parameters "slack-user" :follow #'my/slack-user-org-link-follow))

;; --- Slack Global Logging (SQLite) ---
(add-to-list 'load-path user-emacs-directory)
(require 'my-slack-db)
(my/setup-slack-db-logging)

(global-set-key (kbd "C-c s D") 'my/slack-show-logs)
(global-set-key (kbd "C-c s S") 'my/slack-sync-current-team)
(global-set-key (kbd "C-c s l") 'my/open-latest-slack-log) ;; Keeping this for backward compatibility if needed, or remove if you want to fully switch.
(global-set-key (kbd "C-c s L") 'my/slack-log-message-at-point)

(provide 'init)
;;; init.el ends here
