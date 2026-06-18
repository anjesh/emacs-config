;;; my-treemacs.el --- Treemacs configuration and helpers -*- lexical-binding: t; -*-

(require 'use-package)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
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
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (add-hook 'treemacs-delete-workspace-functions
            #'my/treemacs-delete-frames-for-workspace)
  (define-key treemacs-mode-map (kbd "O") #'my/open-in-external-app)
  (define-key treemacs-mode-map (kbd "T") #'my/open-ghostty-here)
  (define-key treemacs-mode-map (kbd "v") #'my/open-vterm-here)
  (define-key treemacs-mode-map (kbd "V") #'my/open-vterm-here-side)
  (define-key treemacs-mode-map (kbd "E") #'my/open-eshell-here)
  (define-key treemacs-mode-map (kbd "C") #'my/treemacs-copy-path-to-clipboard)
  (define-key treemacs-mode-map (kbd "L") #'org-store-link)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(defun my/treemacs--buffer-path (buffer)
  "Return BUFFER's relevant workspace path, if any."
  (with-current-buffer buffer
    (or (and-let* ((file (buffer-file-name (or (buffer-base-buffer) buffer))))
          (ignore-errors (file-truename file)))
        (and default-directory
             (ignore-errors (file-truename default-directory))))))

(defun my/treemacs-workspace-project-paths (workspace)
  "Return normalized project paths for Treemacs WORKSPACE."
  (mapcar (lambda (project)
            (file-name-as-directory
             (file-truename (treemacs-project->path project))))
          (treemacs-workspace->projects workspace)))

(defun my/treemacs-workspace-buffers (workspace)
  "Return buffers associated with Treemacs WORKSPACE."
  (let* ((frames (my/treemacs-frames-for-workspace workspace))
         (project-paths (my/treemacs-workspace-project-paths workspace))
         buffers)
    (dolist (frame frames)
      (dolist (buffer (if (fboundp 'beframe-buffer-list)
                          (beframe-buffer-list frame)
                        (mapcar #'window-buffer (window-list frame 'no-minibuf frame))))
        (when (buffer-live-p buffer)
          (push buffer buffers))))
    (dolist (buffer (buffer-list))
      (when-let* ((buffer-path (my/treemacs--buffer-path buffer)))
        (when (seq-some (lambda (project-path)
                          (file-in-directory-p buffer-path project-path))
                        project-paths)
          (push buffer buffers))))
    (delq nil (delete-dups buffers))))

(defun my/treemacs-kill-workspace-buffers (workspace)
  "Kill buffers associated with Treemacs WORKSPACE.

Buffers still displayed in non-workspace frames are preserved."
  (let* ((target-workspace (or workspace (treemacs-current-workspace)))
         (frames (my/treemacs-frames-for-workspace target-workspace))
         (killed 0))
    (dolist (buffer (my/treemacs-workspace-buffers target-workspace))
      (when (and (buffer-live-p buffer)
                 (not (seq-some
                       (lambda (window)
                         (not (memq (window-frame window) frames)))
                       (get-buffer-window-list buffer nil t))))
        (kill-buffer buffer)
        (setq killed (1+ killed))))
    killed))

(defun my/treemacs-delete-frames-for-workspace (&optional workspace)
  "Delete all frames assigned to Treemacs WORKSPACE."
  (interactive
   (list (treemacs--select-workspace-by-name)))
  (let* ((target-workspace (or workspace (treemacs-current-workspace)))
         (frames (my/treemacs-frames-for-workspace target-workspace))
         (killed (my/treemacs-kill-workspace-buffers target-workspace))
         (deleted 0))
    (dolist (frame frames)
      (when (frame-live-p frame)
        (delete-frame frame)
        (setq deleted (1+ deleted))))
    (when (called-interactively-p 'interactive)
      (message "Deleted %d frame(s) and killed %d buffer(s) for Treemacs workspace %s."
               deleted
               killed
               (treemacs-workspace->name target-workspace)))))

(defun my/treemacs-frames-for-workspace (&optional workspace)
  "Return live frames associated with Treemacs WORKSPACE."
  (let ((target-workspace (or workspace (treemacs-current-workspace))))
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((frame (car entry))
                   (shelf (cdr entry)))
               (when (and (frame-live-p frame)
                          (eq (treemacs-scope-shelf->workspace shelf)
                              target-workspace))
                 frame)))
           (treemacs--scope-store)))))

(defun my/treemacs-list-frames-for-workspace (&optional workspace)
  "Display frames associated with Treemacs WORKSPACE."
  (interactive
   (list (treemacs--select-workspace-by-name)))
  (let* ((target-workspace (or workspace (treemacs-current-workspace)))
         (frames (my/treemacs-frames-for-workspace target-workspace))
         (buffer (get-buffer-create "*Treemacs Workspace Frames*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Workspace: %s\nFrames: %d\n\n"
                      (treemacs-workspace->name target-workspace)
                      (length frames)))
      (if frames
          (dolist (frame frames)
            (insert
             (format "- %s  name=%S  terminal=%S\n"
                     frame
                     (frame-parameter frame 'name)
                     (frame-terminal frame))))
        (insert "No live frames are currently associated with this workspace.\n")))
    (display-buffer buffer)))

(defun my/treemacs-copy-path-to-clipboard ()
  "Copy the full path of the current file, dired-marked file, or Treemacs node."
  (interactive)
  (let ((path (cond
               ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
               ((derived-mode-p 'treemacs-mode)
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

(provide 'my-treemacs)
;;; my-treemacs.el ends here
