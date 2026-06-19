;;; my-project.el --- project.el configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dired)
(require 'package)
(require 'project)
(require 'subr-x)
(require 'use-package)
(require 'vc)
(declare-function dired-sidebar-buffer "ext:dired-sidebar" ())
(declare-function vterm "ext:vterm" (&optional arg))

(cl-defstruct my/project-ignore-rule
  pattern
  negated
  directory-only
  anchored)

(defun my/project-try-dot-project (dir)
  "Treat the nearest ancestor containing a .project file as a project root."
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (cons 'dot-project (expand-file-name root)))))

(cl-defmethod project-root ((project (head dot-project)))
  (cdr project))

(defun my/project-vterm ()
  "Open `vterm' in the current project root."
  (interactive)
  (require 'vterm)
  (let ((default-directory (project-root (project-current t))))
    (vterm t)))

(defun my/ensure-dired-sidebar-installed ()
  "Install `dired-sidebar' from GitHub when missing."
  (unless (package-installed-p 'dired-sidebar)
    (package-vc-install
     '(dired-sidebar :url "https://github.com/jojojames/dired-sidebar"))))

(use-package dired-sidebar
  :ensure nil
  :preface
  (my/ensure-dired-sidebar-installed)
  :commands (dired-sidebar-toggle-sidebar
             dired-sidebar-jump-to-sidebar)
  :custom
  (dired-sidebar-theme 'icons)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-window-fixed nil)
  (dired-sidebar-width 32))

(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons dired-sidebar))

(defcustom my/project-sidebar-hide-gitignored-files t
  "Whether project sidebars should hide files ignored by `.gitignore' by default."
  :type 'boolean
  :group 'project)

(defvar-local my/project-sidebar-hide-ignored-files
  my/project-sidebar-hide-gitignored-files
  "Whether the current project sidebar hides files ignored by `.gitignore'.")

(defvar-local my/project-sidebar--ignored-overlays nil
  "Overlays used to hide ignored entries in the current sidebar buffer.")

(defun my/project-sidebar-open (&optional root)
  "Show `dired-sidebar' for project ROOT or the current project."
  (interactive)
  (require 'dired-sidebar)
  (let ((default-directory (or root (project-root (project-current t)))))
    (unless (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-toggle-sidebar))
    (dired-sidebar-jump-to-sidebar)
    (my/project-sidebar--refresh-ignored-visibility)))

(defun my/project-sidebar-close ()
  "Close the current `dired-sidebar'."
  (interactive)
  (require 'dired-sidebar)
  (when (dired-sidebar-showing-sidebar-p)
    (dired-sidebar-toggle-sidebar)))

(defun my/project-sidebar-toggle ()
  "Toggle `dired-sidebar' for the current project."
  (interactive)
  (require 'dired-sidebar)
  (let ((default-directory (project-root (project-current t))))
    (dired-sidebar-toggle-sidebar)
    (when (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-jump-to-sidebar))))

(defun my/project-sidebar-open-after-switch (&rest _)
  "Open the project sidebar after switching projects."
  (when-let* ((project (project-current nil)))
    (my/project-sidebar-open (project-root project))))

(defun my/project-sidebar--project-root ()
  "Return the current sidebar project root, or nil when unavailable."
  (when-let* ((project (project-current nil)))
    (project-root project)))

(defun my/project-sidebar--clear-ignored-overlays ()
  "Remove overlays used to hide ignored entries in the current sidebar."
  (mapc #'delete-overlay my/project-sidebar--ignored-overlays)
  (setq my/project-sidebar--ignored-overlays nil))

(defun my/project-sidebar--hide-current-line ()
  "Hide the current line in the sidebar."
  (let ((overlay (make-overlay (line-beginning-position)
                               (min (point-max) (1+ (line-end-position))))))
    (overlay-put overlay 'invisible t)
    (push overlay my/project-sidebar--ignored-overlays)))

(defun my/project-sidebar--refresh-ignored-visibility ()
  "Hide ignored files in the current project sidebar when enabled."
  (when (derived-mode-p 'dired-sidebar-mode)
    (my/project-sidebar--clear-ignored-overlays)
    (when my/project-sidebar-hide-ignored-files
      (when-let* ((root (my/project-sidebar--project-root)))
        (let ((root (file-name-as-directory root))
              (rules-cache (make-hash-table :test #'equal)))
          (save-excursion
            (goto-char (point-min))
            (while (< (point) (point-max))
              (let ((file (ignore-errors (dired-get-filename nil t))))
                (when (and file
                           (not (equal (file-name-as-directory file) root))
                           (my/project--ignored-p root
                                                  file
                                                  (file-directory-p file)
                                                  rules-cache))
                  (my/project-sidebar--hide-current-line))
                (forward-line 1)))))))))

(defun my/project-sidebar--setup ()
  "Set up project-specific behavior for `dired-sidebar' buffers."
  (setq-local my/project-sidebar-hide-ignored-files
              my/project-sidebar-hide-gitignored-files)
  (add-hook 'dired-after-readin-hook
            #'my/project-sidebar--refresh-ignored-visibility
            nil
            t))

(defun my/project-sidebar-toggle-gitignored-files ()
  "Toggle whether the project sidebar shows files ignored by `.gitignore'."
  (interactive)
  (require 'dired-sidebar)
  (let ((buffer (if (derived-mode-p 'dired-sidebar-mode)
                    (current-buffer)
                  (dired-sidebar-buffer))))
    (unless buffer
      (user-error "No project sidebar is open"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'dired-sidebar-mode)
        (user-error "Sidebar buffer is not active"))
      (setq-local my/project-sidebar-hide-ignored-files
                  (not my/project-sidebar-hide-ignored-files))
      (my/project-sidebar--refresh-ignored-visibility)
      (message "Project sidebar ignored files %s"
               (if my/project-sidebar-hide-ignored-files
                   "hidden"
                 "shown")))))

(defun my/project--gitignore-rules (dir)
  "Return parsed .gitignore rules from DIR."
  (let ((gitignore (expand-file-name ".gitignore" dir))
        rules)
    (when (file-exists-p gitignore)
      (with-temp-buffer
        (insert-file-contents gitignore)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let* ((trimmed (string-trim-right line))
                 (negated (string-prefix-p "!" trimmed))
                 (content (if negated (substring trimmed 1) trimmed)))
            (unless (or (string-empty-p content)
                        (string-prefix-p "#" content))
              (push (make-my/project-ignore-rule
                     :pattern (if (string-prefix-p "/" content)
                                  (substring content 1)
                                content)
                     :negated negated
                     :directory-only (string-suffix-p "/" content)
                     :anchored (string-prefix-p "/" content))
                    rules))))))
    (nreverse rules)))

(defun my/project--gitignore-rule-matches-p (rule rel-path is-directory)
  "Return non-nil when RULE matches REL-PATH.
IS-DIRECTORY should be non-nil when REL-PATH names a directory."
  (let* ((directory-only (my/project-ignore-rule-directory-only rule))
         (pattern (string-remove-suffix "/"
                                        (my/project-ignore-rule-pattern rule)))
         (anchored (my/project-ignore-rule-anchored rule))
         (parts (split-string rel-path "/" t))
         (name (car (last parts)))
         (case-fold-search nil))
    (when (or (not directory-only) is-directory)
      (if (string-match-p "/" pattern)
          (or (string-match-p
               (concat "\\`" (wildcard-to-regexp pattern) "\\'")
               rel-path)
              (and directory-only
                   (string-prefix-p (file-name-as-directory pattern)
                                    (file-name-as-directory rel-path))))
        (or (string-match-p
             (concat "\\`" (wildcard-to-regexp pattern) "\\'")
             name)
            (and (not anchored)
                 directory-only
                 (cl-some
                  (lambda (part)
                    (string-match-p
                     (concat "\\`" (wildcard-to-regexp pattern) "\\'")
                     part))
                  parts)))))))

(defun my/project--ignored-p (root path is-directory rules-cache)
  "Return non-nil when PATH should be ignored under ROOT.
RULES-CACHE memoizes parsed .gitignore files by directory."
  (let ((result nil)
        (dir (if is-directory
                 path
               (file-name-directory path))))
    (dolist (ancestor (reverse
                       (cl-loop for current = dir then parent
                                for parent = (file-name-directory
                                              (directory-file-name current))
                                while (and current
                                           (file-in-directory-p current root))
                                collect current
                                until (equal (file-name-as-directory current)
                                             (file-name-as-directory root)))))
      (let* ((rules (or (gethash ancestor rules-cache)
                        (puthash ancestor
                                 (my/project--gitignore-rules ancestor)
                                 rules-cache)))
             (rel-path (file-relative-name path ancestor)))
        (dolist (rule rules)
          (when (my/project--gitignore-rule-matches-p
                 rule
                 rel-path
                 is-directory)
            (setq result (not (my/project-ignore-rule-negated rule)))))))
    result))

(defun my/project--list-local-files (root)
  "List files in ROOT while respecting nested `.gitignore' files."
  (let ((rules-cache (make-hash-table :test #'equal))
        (results nil))
    (cl-labels
        ((walk (dir)
           (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp t))
             (cond
              ((member (file-name-nondirectory entry) vc-directory-exclusion-list))
              ((file-directory-p entry)
               (unless (my/project--ignored-p root entry t rules-cache)
                 (walk entry)))
              ((file-regular-p entry)
               (unless (my/project--ignored-p root entry nil rules-cache)
                 (push entry results)))))))
      (walk root))
    (nreverse results)))

(cl-defmethod project-files ((project (head dot-project)) &optional dirs)
  "List files for PROJECT, honoring VC ignores when available."
  (mapcan
   (lambda (dir)
     (let ((backend (vc-responsible-backend dir 'no-error)))
       (if backend
           (progn
             (require (intern (format "vc-%s" (downcase (symbol-name backend)))))
             (vc-call-backend backend 'project-list-files dir nil))
         (my/project--list-local-files dir))))
   (or dirs
       (list (project-root project)))))

;; Put .project detection ahead of VC so nested marker-based projects
;; inside a larger repository can take precedence over the repo root.
(add-hook 'project-find-functions #'my/project-try-dot-project)
(add-hook 'dired-sidebar-mode-hook #'my/project-sidebar--setup)

(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-regexp "Find regexp")
        (project-find-dir "Find directory")
        (project-vc-dir "VC-Dir")
        (my/project-vterm "vterm" ?t)
        (project-any-command "Other")))

(define-key project-prefix-map (kbd "t") #'my/project-vterm)
(define-key project-prefix-map (kbd "I") #'my/project-sidebar-toggle-gitignored-files)
(define-key project-prefix-map (kbd "s") #'my/project-sidebar-toggle)

(with-eval-after-load 'dired-sidebar
  (define-key dired-sidebar-mode-map (kbd "I")
              #'my/project-sidebar-toggle-gitignored-files))

(advice-add 'project-switch-project :after #'my/project-sidebar-open-after-switch)

(provide 'my-project)
;;; my-project.el ends here
