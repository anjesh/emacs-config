;;; my-agent-shell-magit.el --- Magit helpers for agent-shell -*- lexical-binding: t; -*-

(require 'subr-x)

(declare-function agent-shell-start "agent-shell" (&key config session-id outgoing-request-decorator))
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-queue-request "agent-shell" (prompt))
(declare-function agent-shell--clean-up "agent-shell")
(declare-function agent-shell-openai-make-codex-config "agent-shell-openai")
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-status "magit")
(declare-function magit-dispatch "magit")
(declare-function magit-toplevel "magit")
(declare-function magit-commit-create "magit-commit")
(declare-function my/agent-shell--set-buffer-cwd "my-agent-shell" (shell-buffer dir))
(declare-function shell-maker-busy "shell-maker")
(declare-function shell-maker-last-output "shell-maker")

(defun my/magit-agent-shell--ensure-magit ()
  "Load Magit support on demand."
  (unless (require 'magit nil t)
    (user-error "Magit is not available")))

(defun my/magit-agent-shell--diff (&optional unstaged)
  "Return the current repo diff as a string.

Use the staged diff by default. With UNSTAGED, use the working tree diff."
  (my/magit-agent-shell--ensure-magit)
  (let ((default-directory (or (magit-toplevel)
                               default-directory)))
    (with-temp-buffer
      (magit-git-insert "diff" "--stat" (unless unstaged "--cached"))
      (let ((stat (string-trim (buffer-string))))
        (erase-buffer)
        (magit-git-insert "diff" "--no-ext-diff" "--minimal"
                          (unless unstaged "--cached"))
        (let ((patch (string-trim (buffer-string))))
          (string-join
           (delq nil
                 (list
                  (unless (string-empty-p stat) stat)
                  (unless (string-empty-p patch) patch)))
           "\n\n"))))))

(defun my/git-commit--message-region-end ()
  "Return the end of the editable commit message region."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^%s" (regexp-quote comment-start)) nil t)
        (line-beginning-position)
      (point-max))))

(defun my/magit-agent-shell--repo-root ()
  "Return the current Git repository root."
  (my/magit-agent-shell--ensure-magit)
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun my/magit-agent-shell--commit-buffer (&optional repo-root)
  "Return a live commit message buffer for REPO-ROOT, if any."
  (let ((repo-root (file-name-as-directory
                    (expand-file-name (or repo-root (my/magit-agent-shell--repo-root))))))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (and (bound-and-true-p git-commit-mode)
              default-directory
              (string-prefix-p repo-root
                               (file-name-as-directory
                                (expand-file-name default-directory))))))
     (buffer-list))))

(defun my/magit-agent-shell--start-codex-at-directory (dir)
  "Start a new temporary Codex `agent-shell' session rooted at DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (default-directory dir)
         (agent-shell-cwd-function (lambda () dir))
         (shell-buffer (agent-shell-start
                        :config (agent-shell-openai-make-codex-config))))
    (my/agent-shell--set-buffer-cwd shell-buffer dir)
    shell-buffer))

(defun my/git-commit--replace-message (message)
  "Replace the editable commit message region with MESSAGE."
  (let ((inhibit-read-only t)
        (message (string-trim message)))
    (save-excursion
      (delete-region (point-min) (my/git-commit--message-region-end))
      (goto-char (point-min))
      (insert message)
      (unless (string-suffix-p "\n" message)
        (insert "\n"))
      (unless (looking-at-p "\n\\|\\'")
        (insert "\n")))))

(defun my/git-commit--set-pending-message ()
  "Replace the editable commit message region with a temporary placeholder."
  (my/git-commit--replace-message
   "Generating commit message with Codex..."))

(defun my/magit-agent-shell--latest-response (shell-buffer)
  "Return the latest response text from SHELL-BUFFER."
  (with-current-buffer shell-buffer
    (string-trim (or (shell-maker-last-output) ""))))

(defun my/magit-agent-shell--insert-commit-message-when-ready (shell-buffer target-buffer)
  "Insert the latest `agent-shell' response from SHELL-BUFFER into TARGET-BUFFER."
  (when (and (buffer-live-p shell-buffer)
             (buffer-live-p target-buffer))
    (with-current-buffer shell-buffer
      (if (shell-maker-busy)
          (run-at-time 1 nil #'my/magit-agent-shell--insert-commit-message-when-ready
                       shell-buffer target-buffer)
        (let ((response (my/magit-agent-shell--latest-response shell-buffer)))
          (if (string-empty-p response)
              (message "agent-shell returned an empty commit message")
            (with-current-buffer target-buffer
              (my/git-commit--replace-message response)
              (message "Inserted commit message from agent-shell"))))
        (ignore-errors (agent-shell--clean-up))
        (kill-buffer shell-buffer)))))

(defun my/magit-agent-shell--request-commit-message (target-buffer &optional unstaged)
  "Generate a commit message for the current diff and insert it into TARGET-BUFFER.

Use the staged diff by default. With UNSTAGED, use the working tree diff."
  (unless (require 'agent-shell nil t)
    (user-error "agent-shell is not available"))
  (let* ((repo-root (my/magit-agent-shell--repo-root))
         (diff (my/magit-agent-shell--diff unstaged))
         (diff-kind (if unstaged "unstaged" "staged")))
    (when (string-empty-p diff)
      (user-error "No %s diff found" diff-kind))
    (with-current-buffer target-buffer
      (my/git-commit--set-pending-message))
    (let ((shell-buffer (my/magit-agent-shell--start-codex-at-directory repo-root)))
      (with-current-buffer shell-buffer
        (agent-shell-queue-request
         (format
          (concat "Write a concise Git commit message for this %s diff.\n"
                  "Return only the commit message text with no code fences or commentary.\n"
                  "Use a Conventional Commit subject line and include a short body only if needed.\n\n"
                  "Diff:\n```diff\n%s\n```")
          diff-kind
          diff)))
      (my/magit-agent-shell--insert-commit-message-when-ready shell-buffer target-buffer)
      (message "Asked agent-shell to draft the commit message..."))))

(defun my/git-commit-agent-shell-fill-message (&optional unstaged)
  "Generate and insert a commit message into the current commit buffer."
  (interactive "P")
  (unless (bound-and-true-p git-commit-mode)
    (user-error "Not in a git commit message buffer"))
  (my/magit-agent-shell--request-commit-message (current-buffer) unstaged))

(defun my/magit-agent-shell-commit-message (&optional unstaged)
  "Open a commit buffer and fill it using `agent-shell'.

Use the staged diff by default. With prefix arg UNSTAGED, use the working tree
diff instead."
  (interactive "P")
  (my/magit-agent-shell--ensure-magit)
  (let* ((repo-root (my/magit-agent-shell--repo-root))
         (commit-buffer (or (my/magit-agent-shell--commit-buffer repo-root)
                            (progn
                              (call-interactively #'magit-commit-create)
                              (or (my/magit-agent-shell--commit-buffer repo-root)
                                  (current-buffer))))))
    (unless (buffer-live-p commit-buffer)
      (user-error "Could not find commit message buffer"))
    (pop-to-buffer commit-buffer)
    (my/magit-agent-shell--request-commit-message commit-buffer unstaged)))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c A c")
              #'my/magit-agent-shell-commit-message))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-c A c")
              #'my/git-commit-agent-shell-fill-message))

(provide 'my-agent-shell-magit)

;;; my-agent-shell-magit.el ends here
