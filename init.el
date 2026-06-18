;;; init.el --- User configuration file  -*- lexical-binding: t; -*-

;; Enable Syntax Highlighting Early
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Initialize Package Manager and MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Clean up UI & Silent Bell (Mode Line Flash)
(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#F2804F") ;; Flash Orange
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

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
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; --- Your Original Configuration ---

(use-package ess
  :ensure t
  :defer t)

(use-package ob-mermaid
  :ensure t
  :defer t
  :init
  (setq ob-mermaid-cli-path "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/mmdc"))

(defvar my/org-babel-languages-loaded nil
  "Whether Org Babel languages have been loaded for this session.")

(defun my/org-babel-ensure-languages (&rest _)
  "Load configured Org Babel languages on first use."
  (unless my/org-babel-languages-loaded
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    (setq my/org-babel-languages-loaded t)))

;; Load custom settings from custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)



;; --- End Original Configuration ---

  ;; Icons (Required for Neotree icons)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package magit
  :ensure t)

(require 'my-treemacs)

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
  ;; Avoid writing per-project `.projectile-cache.eld` files.
  ;; Use in-memory caching for the current Emacs session instead.
  (setq projectile-enable-caching t)
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

;; Beframe: Isolate buffers per frame
(use-package beframe
  :ensure t
  :config
  (beframe-mode 1)
  
  ;; Integration with Consult to show only frame-specific buffers by default
  (with-eval-after-load 'consult
    (defun my/consult-beframe-buffer-list (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility."
      (beframe-buffer-list frame :sort #'beframe-buffer-sort-visibility))
    (setq consult-buffer-list-function #'my/consult-beframe-buffer-list)))

;; Marginalia: Annotations (file size, mode, etc.) in the minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Org Mode Configuration
(use-package org
  :ensure nil ;; Built-in
  :init
  (setq org-babel-load-languages
        '((shell . t)
          (R . t)
          (python . nil)
          (mermaid . t)))
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
  (setq browse-url-browser-function 'eww-browse-url) ;; Open links in EWW by default
  (setq org-export-with-section-numbers nil) ;; Disable numbered headings globally
  (setq org-hide-leading-stars t) ;; Hide all but the last star

  (defvar my/org-agenda-files-cache nil
    "Cached value for `org-agenda-files'.")

  (defun my/org-compute-agenda-files ()
    "Find agenda files under `org-directory', excluding large unwanted trees."
    (seq-filter
     (lambda (file)
       (not (string-match-p "/\\(journal\\|obsidian-notes\\|client-projects\\|slack\\|research\\|kings\\)/" file)))
     (directory-files-recursively org-directory "\\.org$")))

  (defun my/org-refresh-agenda-files ()
    "Rebuild and cache `org-agenda-files'."
    (interactive)
    (setq my/org-agenda-files-cache (my/org-compute-agenda-files))
    (setq org-agenda-files my/org-agenda-files-cache))

  (defun my/org-ensure-agenda-files (&rest _)
    "Populate `org-agenda-files' on first agenda use."
    (unless my/org-agenda-files-cache
      (my/org-refresh-agenda-files)))

  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-ensure-languages)
  (advice-add 'org-babel-expand-src-block :before #'my/org-babel-ensure-languages)
  (advice-add 'org-agenda :before #'my/org-ensure-agenda-files)
  
  ;; Custom Font Styling for Headers: All normal weight, all same size
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8))
    (set-face-attribute face nil :height 1.0 :weight 'normal))

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

  ;; Compute agenda files lazily on first agenda use instead of during init.
  (setq org-agenda-files nil)
  
  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i@/!)" "WAITING(w@/!)" "FUTURE(f)" "|" "DONE(d@)" "CANCELLED(c@/!)")))
  (setq org-log-into-drawer t)

  ;; Custom TODO keyword faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DOING" :foreground "orange" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("FUTURE" :foreground "gray")
          ("DONE" :foreground "green" :weight bold)
          ("CANCELLED" :foreground "light gray")))

  ;; Show parent headings (breadcrumbs) in Agenda
  (setq org-agenda-start-on-weekday 0) ;; Start week on Sunday
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
           "* TODO %?\n  %i\n  %a")
           ("b" "Bookmark Review" entry (file+headline "~/dev/orgfiles/bookmarks.org" "Web Reviews")
           "* %?\n  %i\n  %c\n  %U")
           ("r" "Reading List" entry (file+headline "~/dev/orgfiles/bookmarks.org" "Reading List")
           "* TODO %?\n  %i\n  %c\n  %U"))))

;; Browse with EWW
(defun my/eww-browse-at-point ()
  "Browse the URL at point using eww."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (if url
        (eww url)
      (message "No URL at point."))))

(global-set-key (kbd "C-c B") 'my/eww-browse-at-point)

;; Bookmark Search
(defun my/search-bookmarks ()
  "Search through bookmarks and reading list using consult-org-heading."
  (interactive)
  (consult-org-heading nil '("~/dev/orgfiles/bookmarks.org")))

(global-set-key (kbd "C-c f b") 'my/search-bookmarks)

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
                           (visual-line-mode 1)))
  :config
  (setq markdown-fontify-code-blocks-natively t))

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

;; --- SQLite Management (Built-in) ---
(use-package sqlite-mode
  :ensure nil ;; Built-in in Emacs 29+
  :mode ("\\.sqlite\\'" "\\.sqlite3\\'" "\\.db\\'")
  :bind ("C-c D o" . sqlite-mode-open-file))

;; --- SQL Management (Multi-Database) ---
(use-package sql
  :ensure nil
  :bind ("C-c D c" . my/sql-connect-preset)
  :config
  ;; Load external configuration from lisp/my-sql-config.el
  (load (expand-file-name "lisp/my-sql-config.el" user-emacs-directory)))

(use-package sql-indent
  :ensure t
  :after sql)

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

(defun my/open-vterm-here ()
  "Open vterm in the current buffer's directory, dired dir, or treemacs node."
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (vterm t))
      (message "Could not determine directory."))))

(defun my/open-vterm-here-side ()
  "Open a *new* vterm in a right side window at the current location.

This is meant to be used from Treemacs (e.g. bound to `V`) so it does not
*toggle* an existing vterm; it always creates a new vterm buffer." 
  (interactive)
  (let ((path (my/current-path-for-terminal)))
    (if (and path (not (string-empty-p path)))
        (let ((default-directory path))
          (require 'vterm)
          ;; Create a new session without stealing the Treemacs window...
          (let* ((buf (vterm--internal #'ignore t))
                 ;; ...then show it on the right.
                 (win (display-buffer-in-side-window
                       buf
                       '((side . right)
                         (window-width . 0.3)))))
            (select-window win)))
      (message "Could not determine directory."))))

(defun my/open-eshell-here ()
  "Open eshell in the current buffer's directory, dired dir, or treemacs node."
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
          (eshell t))
      (message "Could not determine directory."))))

(global-set-key (kbd "C-c t g") 'my/open-ghostty-here)
(global-set-key (kbd "C-c t v") 'my/open-vterm-here)
(global-set-key (kbd "C-c t e") 'my/open-eshell-here)

(use-package ghostel
  :ensure t)

;; Vterm Configuration (Requires cmake & libtool)
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key))
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines t)))
  (setq vterm-max-scrollback 10000))

;; Vterm Toggle (Pop up terminal)
(use-package vterm-toggle
  :ensure t
  :bind ("C-c t V" . vterm-toggle)
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
                 (window-width . 0.4))))

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

;; --- Org Mode Keybinding Fixes ---
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

;; One-word-at-a-time speed-reading mode.
(use-package spray
  :ensure t
  :bind ("<f6>" . spray-mode)
  :config
  (setq spray-wpm 420
        spray-save-point t))

;; nov.el - EPUB Reader
(require 'my-ebooks)

;; --- Window Highlighting (Dimmer) ---
(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.2) ;; Gentle background dimming
  (setq dimmer-adjustment-mode :background) 
  (setq dimmer-use-colors-space :rgb)
  
  ;; IMPORTANT: Ensure it works on splits within the same frame
  (setq dimmer-watch-frame-focus-events nil) ;; Don't dim everything when switching apps
  
  ;; Explicitly set the dimmed face to a neutral gray background
  (custom-set-faces
   '(dimmer-dim-face ((t (:background "#e0e0e0")))))
  
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
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
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

  (setq hackernews-internal-browser-function #'my/hackernews-open-url)
  (button-type-put 'hackernews-link 'action #'my/hackernews-browse-url-action)
  (button-type-put 'hackernews-comment-count 'action #'my/hackernews-browse-url-action)
  
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
    "Jump to the 'comments' button and open it in the Hacker News split window."
    (interactive)
    (save-excursion
      (end-of-line)
      (if (search-backward "comments" (line-beginning-position) t)
          (let ((url (get-text-property (point) 'help-echo)))
            (if url
                (my/hackernews-open-url url)
              (message "No URL found for comments.")))
        (message "No comments link found on this line."))))

  :bind
  ("C-c h n" . hackernews)
  (:map hackernews-mode-map
        ("w" . my/hackernews-copy-url)
        ("c" . my/hackernews-open-comments)))

;; --- Gemini CLI Integration ---
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

;; --- Shell Maker (Dependency for Agent Shell / Gemini CLI) ---
(use-package shell-maker
  :ensure t
  :vc (:url "https://github.com/xenodium/shell-maker" :rev :newest))

;; --- ACP (Dependency for Agent Shell) ---
(use-package acp
  :ensure t
  :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))

;; --- Agent Shell Integration ---
;; Ensure agent-shell is installed
(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))

  ;; Custom CWD logic
  (defvar-local my/agent-shell-buffer-cwd nil
    "Buffer-local working directory override for `agent-shell`.")

  (defun my/agent-shell--selected-config ()
    "Return the agent-shell config to use for a new session."
    (or agent-shell-preferred-agent-config
        (agent-shell-select-config :prompt "Start new agent: ")
        (error "No agent config found")))

  (defun my/agent-shell--set-buffer-cwd (shell-buffer dir)
    "Persist DIR as cwd for SHELL-BUFFER."
    (with-current-buffer shell-buffer
      (setq-local my/agent-shell-buffer-cwd dir)
      (setq-local default-directory dir)
      (setq-local agent-shell-cwd-function
                  (lambda ()
                    my/agent-shell-buffer-cwd))))

  (defun my/agent-shell--start-at-directory (dir)
    "Start a new agent-shell session rooted at DIR."
    (let* ((dir (file-name-as-directory (expand-file-name dir)))
           (default-directory dir)
           (agent-shell-cwd-function (lambda () dir))
           (shell-buffer (agent-shell-start
                          :config (my/agent-shell--selected-config))))
      (my/agent-shell--set-buffer-cwd shell-buffer dir)
      shell-buffer))

  (defun my/get-agent-shell-context-path ()
    "Get path from treemacs, dired, or buffer for agent-shell."
    (cond
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
          default-directory))))

  (defun my/agent-shell-dwim (&optional _arg)
    "Start `agent-shell` using context directory when available.

When invoked from Treemacs/Dired/a file buffer, force agent-shell's CWD to that
location and start a *new* agent-shell session so the directory takes effect.

Otherwise, fall back to regular `agent-shell` behavior."
    (interactive "P")
    (let ((ctx (my/get-agent-shell-context-path)))
      (if (and ctx (file-directory-p ctx))
          (my/agent-shell--start-at-directory ctx)
        (call-interactively #'agent-shell))))

  (defun my/agent-shell-start-in-directory (dir)
    "Start agent-shell in a specific directory DIR."
    (interactive
     (list (read-directory-name "Agent Shell in directory: " (my/get-agent-shell-context-path))))
    ;; Start directly to avoid DWIM buffer reuse interfering with the chosen CWD.
    (my/agent-shell--start-at-directory dir))

  (defun my/agent-shell-quit (&optional all)
    "Stop agent-shell.

By default, kill all agent-shell sessions belonging to the current project.
With prefix arg ALL (C-u), kill *all* agent-shell sessions." 
    (interactive "P")
    (require 'agent-shell)
    (let* ((shell-bufs (if all
                           (agent-shell-buffers)
                         (agent-shell-project-buffers)))
           (killed 0))
      (dolist (buf shell-bufs)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (derived-mode-p 'agent-shell-mode)
              (ignore-errors (agent-shell--clean-up))))
          (ignore-errors (kill-buffer buf))
          (setq killed (1+ killed))))
      ;; Best-effort: clean up any orphaned viewport buffers.
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (or (derived-mode-p 'agent-shell-viewport-view-mode)
                      (derived-mode-p 'agent-shell-viewport-edit-mode))
              (ignore-errors (kill-buffer buf))))))
      (message "agent-shell: stopped %d session(s)%s"
               killed
               (if all " (all projects)" ""))))

  ;; Keybindings: Make C-c A a prefix map
  (global-unset-key (kbd "C-c A"))
  (define-prefix-command 'my/agent-shell-map)
  (global-set-key (kbd "C-c A") 'my/agent-shell-map)
  (define-key my/agent-shell-map (kbd "A") 'my/agent-shell-dwim)
  (define-key my/agent-shell-map (kbd "d") 'my/agent-shell-start-in-directory)
  (define-key my/agent-shell-map (kbd "q") 'my/agent-shell-quit))

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

;; --- AI Code Interface ---
(use-package ai-code
  :ensure t
  :bind (("C-c i" . ai-code-menu))
  :config
  ;; Set your preferred backend (e.g., 'claude-code, 'gemini, 'codex, 'aider)
  (ai-code-set-backend 'gemini))

;; --- Python & LSP Configuration ---

(use-package eglot
  :ensure nil ;; Built-in in Emacs 29+
  :hook (python-mode . eglot-ensure)
  :bind (:map python-mode-map
              ("C-c C-d" . eldoc)
              ([C-down-mouse-1] . xref-find-definitions-at-mouse))
  :config
  ;; Ensure python-lsp-server is used if installed
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

;; --- Auto-completion (Corfu) ---

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Quit if no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  :config
  ;; Enable Orderless filtering for Corfu
  (defun my/corfu-enable-orderless ()
    (setq-local completion-styles '(orderless basic)))
  (add-hook 'corfu-mode-hook #'my/corfu-enable-orderless)

  ;; Terminal support for Corfu
  (use-package corfu-terminal
    :ensure t
    :if (and (not (display-graphic-p))
             (< emacs-major-version 31))
    :config
    (corfu-terminal-mode +1)))

;; --- Slack Integration ---
(require 'my-slack)

(provide 'init)
;;; init.el ends here
