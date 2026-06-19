;;; my-magit.el --- Magit configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package magit
  :ensure t
  :commands (magit magit-status magit-dispatch)
  :config
  ;; Keep status refreshes and diff rendering conservative in large repos.
  (setq magit-refresh-status-buffer nil
        magit-diff-refine-hunk nil
        magit-save-repository-buffers 'dontask
        auto-revert-check-vc-info nil))

(provide 'my-magit)
;;; my-magit.el ends here
