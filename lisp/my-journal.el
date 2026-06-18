;;; my-journal.el --- Journal navigation helpers -*- lexical-binding: t; -*-

(defun my/open-daily-journal ()
  "Open today's daily journal file."
  (interactive)
  (let ((daily-path (format-time-string "~/dev/journal/daily/%Y/%m/%Y-%m-%d.md")))
    (if (file-exists-p daily-path)
        (find-file daily-path)
      (if (y-or-n-p (format "Daily journal not found at %s. Create it? " daily-path))
          (progn
            (make-directory (file-name-directory daily-path) t)
            (find-file daily-path))
        (message "Cancelled.")))))

(defun my/open-weekly-journal ()
  "Open this week's journal file. If today is Sunday, opens next week's file."
  (interactive)
  (let* ((now (decode-time))
         (dow (nth 6 now))
         (target-time (if (= dow 0)
                          (time-add (current-time) (* 24 3600))
                        (current-time)))
         (weekly-path (format-time-string "~/dev/journal/weekly/%Y/week-%V.md" target-time)))
    (if (file-exists-p weekly-path)
        (find-file weekly-path)
      (if (y-or-n-p (format "Weekly journal not found at %s. Create it? " weekly-path))
          (progn
            (make-directory (file-name-directory weekly-path) t)
            (find-file weekly-path))
        (message "Cancelled.")))))

(defun my/open-monthly-journal ()
  "Open this month's journal file."
  (interactive)
  (let ((monthly-path (format-time-string "~/dev/journal/monthly/%Y/%m-%B.org")))
    (if (file-exists-p monthly-path)
        (find-file monthly-path)
      (if (y-or-n-p (format "Monthly journal not found at %s. Create it? " monthly-path))
          (progn
            (make-directory (file-name-directory monthly-path) t)
            (find-file monthly-path)
            (insert "#+TITLE: Monthly Journal - " (format-time-string "%B %Y") "\n#+STARTUP: showall\n\n"))
        (message "Cancelled.")))))

(defun my/insert-daily-journal-entry ()
  "Insert a daily journal template with Review and Time blocks."
  (interactive)
  (let ((date-string (format-time-string "%Y-%m-%d %a")))
    (insert "* " date-string "\n")
    (insert "** Review\n")
    (insert "*** Win\n")
    (insert "*** Gratitude\n")
    (insert "*** Anxiety\n")
    (insert "*** Kindness\n")
    (insert "*** Interesting\n")
    (insert "*** Ishaan\n")
    (insert "*** Deepika\n")
    (insert "** Intermittent\n")
    (insert "*** " (format-time-string "%H:%M") "\n")))

(global-set-key (kbd "C-c j d") #'my/open-daily-journal)
(global-set-key (kbd "C-c j w") #'my/open-weekly-journal)
(global-set-key (kbd "C-c j m") #'my/open-monthly-journal)
(global-set-key (kbd "C-c j n") #'my/insert-daily-journal-entry)

(provide 'my-journal)
;;; my-journal.el ends here
