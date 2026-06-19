;;; my-agent-shell.el --- Agent shell integration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package shell-maker
  :ensure t
  :defer t
  :vc (:url "https://github.com/xenodium/shell-maker" :rev :newest))

(use-package acp
  :ensure t
  :defer t
  :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))

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
  "Get path from dired or the current buffer for agent-shell."
  (cond
   ((derived-mode-p 'dired-mode) (dired-current-directory))
   (t (if buffer-file-name
          (file-name-directory buffer-file-name)
        default-directory))))

(defun my/agent-shell-dwim (&optional _arg)
  "Start `agent-shell` using context directory when available.

When invoked from Dired or a file buffer, force agent-shell's CWD to that
location and start a new agent-shell session so the directory takes effect.
Otherwise, fall back to regular `agent-shell` behavior."
  (interactive "P")
  (require 'agent-shell)
  (let ((ctx (my/get-agent-shell-context-path)))
    (if (and ctx (file-directory-p ctx))
        (my/agent-shell--start-at-directory ctx)
      (call-interactively #'agent-shell))))

(defun my/agent-shell-start-in-directory (dir)
  "Start agent-shell in a specific directory DIR."
  (interactive
   (list (read-directory-name "Agent Shell in directory: " (my/get-agent-shell-context-path))))
  (require 'agent-shell)
  (my/agent-shell--start-at-directory dir))

(defun my/agent-shell-quit (&optional all)
  "Stop agent-shell sessions.

By default, kill all agent-shell sessions belonging to the current project.
With prefix arg ALL (C-u), kill all agent-shell sessions."
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
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (or (derived-mode-p 'agent-shell-viewport-view-mode)
                    (derived-mode-p 'agent-shell-viewport-edit-mode))
            (ignore-errors (kill-buffer buf))))))
    (message "agent-shell: stopped %d session(s)%s"
             killed
             (if all " (all projects)" ""))))

(use-package agent-shell
  :ensure t
  :commands (agent-shell
             agent-shell-start
             agent-shell-buffers
             agent-shell-project-buffers
             agent-shell-select-config
             agent-shell-google-make-authentication)
  :init
  (global-unset-key (kbd "C-c A"))
  (define-prefix-command 'my/agent-shell-map)
  (global-set-key (kbd "C-c A") 'my/agent-shell-map)
  (define-key my/agent-shell-map (kbd "A") #'my/agent-shell-dwim)
  (define-key my/agent-shell-map (kbd "d") #'my/agent-shell-start-in-directory)
  (define-key my/agent-shell-map (kbd "q") #'my/agent-shell-quit)
  :config
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t)))

(require 'my-agent-shell-magit)

(provide 'my-agent-shell)
;;; my-agent-shell.el ends here
