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

;; Enable visual line movement globally
(setq-default line-move-visual t)
(global-visual-line-mode t)

(require 'my-flyspell)

;; Syntax Highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#f2f2f2") ; Very light gray

(require 'my-gui)

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

;; Load custom settings from custom.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)



;; --- End Original Configuration ---
(require 'my-org)

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

(require 'my-eww)

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

(require 'my-terminals)

;; Popup package
(use-package popup :ensure t)

(require 'my-mouse)

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

(require 'my-journal)
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

(require 'my-dimmer)

;; --- Web Browsing (EWW) ---

;; --- Dictionary (Minibuffer) ---
;; (Dictionary configuration removed)

;; --- YAML Support ---
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(require 'my-hackernews)

(require 'my-agent-shell)


;; --- AI Code Interface ---
(use-package ai-code
  :ensure t
  :bind (("C-c i" . ai-code-menu)))

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
