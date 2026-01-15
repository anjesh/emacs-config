(if (and (boundp 'slack-current-team) slack-current-team)
    (let ((active-count 0)
          (total-count 0)
          (rooms (my/slack-team-rooms slack-current-team)))
      (with-current-buffer (get-buffer-create "*Slack Backfill Debug*"))
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Checking backfill candidates for team: %s\n" (oref slack-current-team name)))
        (insert (format "Time: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (format "%-30s %-12s %-12s %-12s %s\n" "Channel Name" "Status" "Created" "Last Active" "ID"))
        (insert (make-string 95 ?-)) "\n"
        
        (dolist (room rooms)
          (setq total-count (1+ total-count))
          (let* ((name (slack-room-name room slack-current-team))
                 (id (oref room id))
                 (active (my/slack-room-active-p room slack-current-team))
                 
                 ;; Creation Date
                 (created-raw (condition-case nil (oref room created) (error nil)))
                 (created-ts (if (stringp created-raw) (string-to-number created-raw) created-raw))
                 (created-str (if (and (numberp created-ts) (> created-ts 0))
                                  (format-time-string "%Y-%m-%d" (seconds-to-time created-ts))
                                "Unknown"))
                 
                 ;; Last Active Date
                 (latest-raw (condition-case nil (slack-room-latest room slack-current-team) (error "0")))
                 (latest-ts (string-to-number (or latest-raw "0")))
                 (latest-str (if (and (numberp latest-ts) (> latest-ts 0))
                                 (format-time-string "%Y-%m-%d" (seconds-to-time latest-ts))
                               "Never")))
            
            (if active (setq active-count (1+ active-count)))
            (insert (format "%-30s %-12s %-12s %-12s %s\n" 
                            (truncate-string-to-width name 28 nil nil t)
                            (if active "[BACKFILL]" "[SKIP]")
                            created-str
                            latest-str
                            id))))
        
        (insert (make-string 95 ?-)) "\n"
        (insert (format "Summary: %d active candidates out of %d total rooms."))
        (display-buffer (current-buffer))))
  (message "Slack not connected or no team selected"))