;;; my-slack.el --- Slack configuration and helpers -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'cl-lib)
(require 'use-package)

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'message))

(defun my/auth-source-pick-first-password (&rest spec)
  "Return the first password matching SPEC from auth-source."
  (let ((res (car (apply #'auth-source-search spec))))
    (if res
        (let ((secret (plist-get res :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "No password found for %S" spec))))

(defun my/slack-org-link-follow (path)
  "Follow a Slack link of the form team-id:room-id or team-id:room-id:thread-ts."
  (require 'slack)
  (let* ((parts (split-string path ":"))
         (team-id (nth 0 parts))
         (room-id (nth 1 parts))
         (thread-ts (nth 2 parts))
         (team (cl-find team-id
                        (hash-table-values slack-teams-by-token)
                        :key (lambda (tm) (oref tm id))
                        :test #'string=))
         (room (when team
                 (or (gethash room-id (oref team channels))
                     (gethash room-id (oref team groups))
                     (gethash room-id (oref team ims))))))
    (if (and team room)
        (if thread-ts
            (slack-open-message team room thread-ts thread-ts)
          (slack-room-display room team))
      (message "Slack link target not found: %s" path))))

(defun my/slack-user-org-link-follow (path)
  "Follow a Slack user link of the form team-id:user-id."
  (require 'slack)
  (let* ((parts (split-string path ":"))
         (team-id (nth 0 parts))
         (user-id (nth 1 parts))
         (team (cl-find team-id
                        (hash-table-values slack-teams-by-token)
                        :key (lambda (tm) (oref tm id))
                        :test #'string=)))
    (if team
        (slack-buffer-display (slack-create-user-profile-buffer team user-id))
      (message "Slack team not found: %s" team-id))))

(with-eval-after-load 'org
  (org-link-set-parameters "slack" :follow #'my/slack-org-link-follow)
  (org-link-set-parameters "slack-user" :follow #'my/slack-user-org-link-follow))

(defun my/slack--ensure-runtime ()
  "Load Slack and the local Slack helpers."
  (require 'slack)
  (require 'my-slack-db))

(defun my/slack-show-logs-command ()
  "Load Slack support and show the Slack logs buffer."
  (interactive)
  (my/slack--ensure-runtime)
  (call-interactively #'my/slack-show-logs))

(defun my/slack-sync-current-team-command ()
  "Load Slack support and sync the current Slack team."
  (interactive)
  (my/slack--ensure-runtime)
  (call-interactively #'my/slack-sync-current-team))

(defun my/slack-manage-channels-command ()
  "Load Slack support and open the Slack channel manager."
  (interactive)
  (my/slack--ensure-runtime)
  (call-interactively #'my/slack-manage-channels))

(defun my/slack-open-latest-log-command ()
  "Load Slack support and open the latest Slack log."
  (interactive)
  (my/slack--ensure-runtime)
  (call-interactively #'my/open-latest-slack-log))

(defun my/slack-log-message-at-point-command ()
  "Load Slack support and log the current Slack message."
  (interactive)
  (my/slack--ensure-runtime)
  (call-interactively #'my/slack-log-message-at-point))

(use-package slack
  :ensure t
  :vc (:url "https://github.com/yuya373/emacs-slack" :rev :newest)
  :commands (slack-start
             slack-message-embed-mention
             slack-ws-close
             slack-select-rooms
             slack-select-unread-rooms
             slack-all-threads)
  :bind (("C-c s s" . slack-start)
         ("C-c s m" . slack-message-embed-mention)
         ("C-c s q" . slack-ws-close)
         ("C-c s r" . slack-select-rooms)
         ("C-c s u" . slack-select-unread-rooms)
         ("C-c s t" . slack-all-threads)
         ("C-c s D" . my/slack-show-logs-command)
         ("C-c s S" . my/slack-sync-current-team-command)
         ("C-c s a" . my/slack-manage-channels-command)
         ("C-c s l" . my/slack-open-latest-log-command)
         ("C-c s L" . my/slack-log-message-at-point-command))
  :init
  (setq slack-buffer-function #'switch-to-buffer)
  (setq slack-prefer-current-team t)
  :config
  (setq slack-enable-notification t)
  (slack-register-team
   :name "younginnovations"
   :default t
   :token (my/auth-source-pick-first-password
           :host "younginnovations.slack.com"
           :user "anjesh@yipl.com.np")
   :cookie (my/auth-source-pick-first-password
            :host "younginnovations.slack.com"
            :user "anjesh@yipl.com.np^cookie"))
  (require 'my-slack-db)
  (my/setup-slack-db-logging))

(provide 'my-slack)
;;; my-slack.el ends here
