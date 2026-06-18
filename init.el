;;; init.el --- User configuration file  -*- lexical-binding: t; -*-

;; --- Bootstrap ---

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-helper)

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

(setq ring-bell-function #'my/flash-mode-line)

(setq-default line-move-visual t)
(global-visual-line-mode t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#f2f2f2")

(require 'my-gui)
(require 'my-flyspell)
(require 'my-dimmer)
(require 'my-mouse)
(require 'my-undo)

;; --- Files And Persistence ---

(require 'my-files)

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

(require 'my-completion)

(use-package beframe
  :ensure t
  :config
  (beframe-mode 1)
  (with-eval-after-load 'my-completion
    (setq consult-buffer-list-function #'my/consult-beframe-buffer-list)))

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
(require 'my-ess)

(use-package popup
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'"))

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

(require 'my-corfu)

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
(global-set-key (kbd "C-c r") #'my/open-user-readme)

(provide 'init)
;;; init.el ends here
