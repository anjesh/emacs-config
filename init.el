;; Initialize Package Manager and MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Enable visual line movement globally
(setq-default line-move-visual t)
(global-visual-line-mode t)

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
          treemacs-width-is-initially-locked     t
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

  ;; Custom TODO keyword faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DOING" :foreground "orange" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("FUTURE" :foreground "gray")
          ("DONE" :foreground "green" :weight bold)
          ("CANCELLED" :foreground "light gray")))

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
  :mode ("\.md\'" . markdown-mode)
  :hook (markdown-mode . (lambda () 
                           (visual-line-mode 1)
                           (adaptive-wrap-prefix-mode 1))) ;; Visually indent wrapped lines
  :config
  ;; --- Performance Fix for Large Files ---
  (defun my/markdown-get-list-start ()
    "Return the start position of the current list block (top-level)."
    (save-excursion
      (beginning-of-line)
      ;; Limit search to 2000 lines back to avoid hangs
      (let ((limit (save-excursion (forward-line -2000) (point))))
        (if (re-search-backward "^[ \t]*\\([-+*]\\|[0-9]+\\.\\)\\s-" limit t)
            (progn
              (while (and (> (current-indentation) 0)
                          (re-search-backward "^[ \t]*\\([-+*]\\|[0-9]+\\.\\)\\s-" limit t)))
              (point))
          (point-min)))))

  (defun my/markdown-limit-context-advice (orig-fun &rest args)
    "Narrow buffer to current top-level list before indenting to improve performance."
    (if (> (buffer-size) 5000) ;; Optimization for large files (>5KB)
        (let ((start (my/markdown-get-list-start)))
          (save-restriction
            (narrow-to-region start (point-max))
            (syntax-propertize (point-max))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'markdown-demote-list-item :around #'my/markdown-limit-context-advice)
  (advice-add 'markdown-promote-list-item :around #'my/markdown-limit-context-advice)
  
  :bind (:map markdown-mode-map
              ))

;; PDF Tools (GUI only)
(use-package pdf-tools
  :ensure t
  :if (display-graphic-p)
  :mode ("\.pdf\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width) ;; or 'fit-page
  (setq pdf-view-continuous t) ;; Continuous scroll
  (add-hook 'pdf-view-mode-hook (lambda () 
                                  (display-line-numbers-mode -1) ;; Disable line numbers in PDF
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

(global-set-key (kbd "C-c t g") 'my/open-ghostty-here)

;; Bind 'O' in Treemacs to open externally, 'T' to open Ghostty, and 'C' to copy path
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "O") #'my/open-in-external-app)
  (define-key treemacs-mode-map (kbd "T") #'my/open-ghostty-here)
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

;; Enable mouse support in terminal (click, scroll, resize)
(xterm-mouse-mode 1)

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
  (let ((date-string (format-time-string "%Y-%m-%d %A")))
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
    (setq shr-max-image-proportion 0.8)))
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
(unless (package-installed-p 'agent-shell)
  (package-refresh-contents)
  (package-install 'agent-shell))

(require 'agent-shell)

;; Configure agent-shell (Force global setq)
(setq agent-shell-agent-configs
      '((:name "Gemini" 
         :command "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/gemini" 
         :args ("chat"))))

(global-set-key (kbd "C-c A") 'agent-shell)

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