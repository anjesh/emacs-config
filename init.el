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
(require 'my-ibuffer)
(require 'my-completion)
(require 'my-beframe)

;; --- Writing, Reading, And Notes ---

(require 'my-org)
(require 'my-journal)
(require 'my-eww)
(require 'my-hackernews)
(require 'my-ebooks)
(require 'my-markdown)
(require 'my-csv)
(require 'my-pdf)
(require 'my-spray)

(use-package adaptive-wrap
  :ensure t
  :defer t)

;; --- Shells, Terminals, And External Tools ---

(require 'my-terminals)
(require 'my-agent-shell)
(require 'my-ess)
(require 'my-yaml)

(use-package popup
  :ensure t)

;; --- Development And Language Tooling ---

(require 'my-ai-code)
(require 'my-eglot)
(require 'my-corfu)
(require 'my-iedit)

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
