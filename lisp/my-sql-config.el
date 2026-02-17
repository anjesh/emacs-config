;;; my-sql-config.el --- SQL Connection Configurations -*- lexical-binding: t; -*-

(require 'sql)

;; Define the connection preset function here so it is available for binding
(defun my/sql-connect-preset ()
  "Connect to a predefined database from `sql-connection-alist`."
  (interactive)
  ;; Ensure sql-indent is loaded if available for better formatting in the interactive buffer
  (require 'sql-indent nil t)
  (call-interactively 'sql-connect))

;; Configure your database connections here
(setq sql-connection-alist
      '((slack-db
         (sql-product 'sqlite)
         (sql-database (expand-file-name "var/slack-messages.db" user-emacs-directory)))))

(provide 'my-sql-config)
