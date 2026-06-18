;;; init.el --- User configuration file  -*- lexical-binding: t; -*-

;; --- Bootstrap ---

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; --- Core UI And Editing ---

(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg))))

(setq-default line-move-visual t)
(global-visual-line-mode t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#f2f2f2")

(require 'my-gui)
(require 'my-flyspell)
(require 'my-dimmer)
(require 'my-mouse)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :bind
  ("C-x u" . undo-tree-visualize))

;; --- Files And Persistence ---

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
    (defun my-copy-to-clipboard (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (defun my-paste-from-clipboard ()
      (shell-command-to-string "pbpaste"))

    (setq interprogram-cut-function 'my-copy-to-clipboard)
    (setq interprogram-paste-function 'my-paste-from-clipboard)))

;; --- Navigation And Project Tools ---

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package magit
  :ensure t)

(require 'my-treemacs)

(use-package ibuffer
  :ensure t
  :init
  (setq ibuffer-expert t)
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

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s"   . consult-line)
         ("M-y"   . consult-yank-pop)
         ("M-g g" . consult-goto-line)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-k" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package beframe
  :ensure t
  :config
  (beframe-mode 1)
  (with-eval-after-load 'consult
    (defun my/consult-beframe-buffer-list (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility."
      (beframe-buffer-list frame :sort #'beframe-buffer-sort-visibility))
    (setq consult-buffer-list-function #'my/consult-beframe-buffer-list)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; --- Writing, Reading, And Notes ---

(require 'my-org)
(require 'my-journal)
(require 'my-eww)
(require 'my-hackernews)
(require 'my-ebooks)
(require 'my-markdown)
(require 'my-csv)
(require 'my-pdf)

(use-package adaptive-wrap
  :ensure t
  :defer t)

(use-package spray
  :ensure t
  :bind ("<f6>" . spray-mode)
  :config
  (setq spray-wpm 420
        spray-save-point t))

;; --- Shells, Terminals, And External Tools ---

(require 'my-terminals)
(require 'my-agent-shell)

(use-package popup
  :ensure t)

(use-package ess
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(defun my/open-in-external-app ()
  "Open the current file, dired-marked file, or treemacs node in external app."
  (interactive)
  (let ((file (cond
               ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
               ((derived-mode-p 'treemacs-mode)
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

;; --- Development And Language Tooling ---

(use-package ai-code
  :ensure t
  :bind (("C-c i" . ai-code-menu)))

(use-package eglot
  :ensure nil
  :hook (python-mode . eglot-ensure)
  :bind (:map python-mode-map
              ("C-c C-d" . eldoc)
              ([C-down-mouse-1] . xref-find-definitions-at-mouse))
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  :config
  (defun my/corfu-enable-orderless ()
    (setq-local completion-styles '(orderless basic)))
  (add-hook 'corfu-mode-hook #'my/corfu-enable-orderless)
  (use-package corfu-terminal
    :ensure t
    :if (and (not (display-graphic-p))
             (< emacs-major-version 31))
    :config
    (corfu-terminal-mode +1)))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; --- Communication And Global Keys ---

(require 'my-slack)

(global-set-key (kbd "C-c n") #'display-line-numbers-mode)
(global-set-key (kbd "C-c o") #'my/open-in-external-app)
(global-set-key (kbd "C-c <") #'shrink-window-horizontally)
(global-set-key (kbd "C-c >") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c -") #'shrink-window)
(global-set-key (kbd "C-c +") #'enlarge-window)
(global-set-key (kbd "C-c u") #'beginning-of-buffer)
(global-set-key (kbd "C-c d") #'end-of-buffer)
(global-set-key
 (kbd "C-c r")
 (lambda ()
   (interactive)
   (find-file (expand-file-name "readme.md" user-emacs-directory))))

(provide 'init)
;;; init.el ends here
