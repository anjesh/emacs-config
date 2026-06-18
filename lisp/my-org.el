;;; my-org.el --- Org mode configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package ob-mermaid
  :ensure t
  :defer t
  :init
  (setq ob-mermaid-cli-path "/Users/anjesh/.nvm/versions/node/v24.2.0/bin/mmdc"))

(defvar my/org-babel-languages-loaded nil
  "Whether Org Babel languages have been loaded for this session.")

(defvar my/org-agenda-files-cache nil
  "Cached value for `org-agenda-files'.")

(defun my/org-babel-ensure-languages (&rest _)
  "Load configured Org Babel languages on first use."
  (unless my/org-babel-languages-loaded
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    (setq my/org-babel-languages-loaded t)))

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

(defun my/toggle-org-bullets ()
  "Toggle between hidden bullets and visible stars."
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

(defun my/search-bookmarks ()
  "Search bookmarks and reading list using `consult-org-heading'."
  (interactive)
  (consult-org-heading nil '("~/dev/orgfiles/bookmarks.org")))

(use-package org-bullets
  :ensure t
  :defer t
  :init
  (setq org-bullets-bullet-list '(" ")))

(use-package org
  :ensure nil
  :init
  (setq org-babel-load-languages
        '((shell . t)
          (R . t)
          (python . nil)
          (mermaid . t)))
  :hook (org-mode . (lambda ()
                      (font-lock-mode 1)
                      (font-lock-ensure)
                      (visual-line-mode 1)
                      (org-indent-mode 1)))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)
         ("C-c f b" . my/search-bookmarks)
         :map org-mode-map
         ("C-c b" . my/toggle-org-bullets))
  :config
  (setq org-directory "~/dev")
  (setq browse-url-browser-function 'eww-browse-url)
  (setq org-export-with-section-numbers nil)
  (setq org-hide-leading-stars t)
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-ensure-languages)
  (advice-add 'org-babel-expand-src-block :before #'my/org-babel-ensure-languages)
  (advice-add 'org-agenda :before #'my/org-ensure-agenda-files)
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                  org-level-5 org-level-6 org-level-7 org-level-8))
    (set-face-attribute face nil :height 1.0 :weight 'normal))
  (setq org-agenda-files nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i@/!)" "WAITING(w@/!)" "FUTURE(f)" "|" "DONE(d@)" "CANCELLED(c@/!)")))
  (setq org-log-into-drawer t)
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("DOING" :foreground "orange" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("FUTURE" :foreground "gray")
          ("DONE" :foreground "green" :weight bold)
          ("CANCELLED" :foreground "light gray")))
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c %b%?-12t% s")
          (todo   . " %i %-12:c %b")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("w" "Work Task" entry (file+headline "~/dev/orgfiles/work.org" "Work Tasks")
           "* TODO %?\n  %i\n  %a")
          ("p" "Personal Task" entry (file+headline "~/dev/orgfiles/personal.org" "Personal Tasks")
           "* TODO %?\n  %i\n  %a")
          ("b" "Bookmark Review" entry (file+headline "~/dev/orgfiles/bookmarks.org" "Web Reviews")
           "* %?\n  %i\n  %c\n  %U")
          ("r" "Reading List" entry (file+headline "~/dev/orgfiles/bookmarks.org" "Reading List")
           "* TODO %?\n  %i\n  %c\n  %U")))
  (define-key org-mode-map (kbd "TAB") #'org-cycle)
  (define-key org-mode-map (kbd "<tab>") #'org-cycle)
  (define-key org-mode-map (kbd "M-<left>") #'org-metaleft)
  (define-key org-mode-map (kbd "M-<right>") #'org-metaright)
  (define-key org-mode-map (kbd "C-c <left>") #'org-metaleft)
  (define-key org-mode-map (kbd "C-c <right>") #'org-metaright)
  (define-key org-mode-map (kbd "C-c <up>") #'org-metaup)
  (define-key org-mode-map (kbd "C-c <down>") #'org-metadown))

(provide 'my-org)
;;; my-org.el ends here
