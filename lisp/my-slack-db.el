;;; my-slack-db.el --- Slack logging to SQLite -*- lexical-binding: t; -*-

(require 'sqlite)
(require 'cl-lib)
(require 'slack)
(require 'slack-message)
(require 'slack-conversations)

(defvar my/slack-db-path (expand-file-name "var/slack-messages.db" user-emacs-directory))

(defcustom my/slack-ignored-channels '("cm-tea-time")
  "List of channel names (strings) to ignore for DB logging."
  :type '(repeat string)
  :group 'slack)

(defun my/slack-room-ignored-p (room team)
  "Return t if ROOM in TEAM should be ignored."
  (let ((name (slack-room-name room team)))
    (member name my/slack-ignored-channels)))

(defun my/slack-room-active-p (room team)
  "Return t if ROOM is active (not ignored, not archived, open, and member)."
  (and (not (my/slack-room-ignored-p room team))
       (not (slack-room-archived-p room))
       (slack-room-open-p room)
       (slack-room-member-p room)))

(defun my/slack-db-init ()
  "Initialize the Slack SQLite database."
  (unless (file-exists-p (file-name-directory my/slack-db-path))
    (make-directory (file-name-directory my/slack-db-path) t))
  (let ((db (sqlite-open my/slack-db-path)))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS messages (
        ts TEXT,
        team_id TEXT,
        room_id TEXT,
        user_id TEXT,
        sender_name TEXT,
        text TEXT,
        thread_ts TEXT,
        json TEXT,
        PRIMARY KEY (ts, team_id, room_id)
      )")
    (sqlite-close db)))

(defun my/slack-db-insert-message (message team room-id)
  "Insert a slack MESSAGE from TEAM in ROOM-ID into the database."
  (let* ((ts (oref message ts))
         (user-id (if (slot-boundp message 'user) (oref message user) nil))
         (text (condition-case nil (slack-message-get-text message team) (error "")))
         (sender-name (or (condition-case nil (slack-message-sender-name message team) (error nil)) user-id "Unknown"))
         (thread-ts (if (slot-boundp message 'thread-ts) (oref message thread-ts) nil))
         (team-id (oref team id))
         (json "")) 
    (let ((db (sqlite-open my/slack-db-path)))
      (condition-case err
          (sqlite-execute db "INSERT OR IGNORE INTO messages (ts, team_id, room_id, user_id, sender_name, text, thread_ts, json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                          (list ts team-id room-id user-id sender-name text thread-ts json))
        (error (message "Error inserting slack message: %S" err)))
      (sqlite-close db))))

(defun my/slack-get-latest-ts (team-id room-id)
  "Get the latest timestamp for TEAM-ID and ROOM-ID from DB."
  (let ((db (sqlite-open my/slack-db-path))
        (result nil))
    (setq result (sqlite-select db "SELECT MAX(ts) FROM messages WHERE team_id = ? AND room_id = ?" (list team-id room-id)))
    (sqlite-close db)
    (if (and result (car result) (car (car result)))
        (car (car result))
      nil)))

(defun my/slack-team-rooms (team)
  "Get all rooms (channels, groups, ims) for TEAM."
  (append (hash-table-values (oref team channels))
          (hash-table-values (oref team groups))
          (hash-table-values (oref team ims))))

(defun my/slack-start-of-today ()
  "Return the timestamp for the start of the current day (00:00:00)."
  (let ((decoded (decode-time)))
    (setf (nth 0 decoded) 0) ; second
    (setf (nth 1 decoded) 0) ; minute
    (setf (nth 2 decoded) 0) ; hour
    (format-time-string "%s" (apply #'encode-time decoded))))

(defun my/slack-ts-greater-p (ts1 ts2)
  "Return t if TS1 is strictly greater than TS2. Handles nil as 0."
  (> (string-to-number (or ts1 "0"))
     (string-to-number (or ts2 "0"))))

(defun my/slack-get-unread-rooms (team)
  "Return a list of rooms in TEAM that have unread messages."
  (cl-remove-if-not
   (lambda (room) (slack-room-has-unread-p room team))
   (my/slack-team-rooms team)))

(defun my/slack-get-rooms-updated-since (team timestamp)
  "Return a list of rooms in TEAM updated after TIMESTAMP."
  (cl-remove-if-not
   (lambda (room)
     (my/slack-ts-greater-p (slack-room-latest room team) timestamp))
   (my/slack-team-rooms team)))

(defun my/slack-get-all-db-latest-ts (team-id)
  "Return a hash table of room-id -> latest-ts for the given TEAM-ID."
  (let ((db (sqlite-open my/slack-db-path))
        (ht (make-hash-table :test 'equal)))
    (condition-case nil
        (let ((rows (sqlite-select db "SELECT room_id, MAX(ts) FROM messages WHERE team_id = ? GROUP BY room_id" (list team-id))))
          (dolist (row rows)
            (puthash (nth 0 row) (nth 1 row) ht)))
      (error nil))
    (sqlite-close db)
    ht))

(defun my/slack-backfill-room (room team)
  "Fetch history for ROOM in TEAM, starting from latest known TS."
  (let* ((team-id (oref team id))
         (room-id (oref room id))
         (latest-ts (my/slack-get-latest-ts team-id room-id))
         (oldest (or latest-ts (my/slack-start-of-today))))
    
    (message "Backfilling %s (from %s)..." (slack-room-name room team) (if latest-ts latest-ts "today"))
    
    (slack-conversations-history 
     room team
     :limit "200"
     :oldest oldest
     :after-success
     (lambda (messages next-cursor)
       (let ((count 0))
         (dolist (msg messages)
           (my/slack-db-insert-message msg team room-id)
           (cl-incf count))
         (message "Backfilled %d messages for %s" count (slack-room-name room team)))))))

(defun my/slack-backfill-team (team)
  "Backfill history for all rooms in TEAM (Legacy/Full)."
  (when team
    (message "Starting full backfill for team %s..." (oref team name))
    (dolist (room (my/slack-team-rooms team))
      ;; Add a small delay between requests to avoid rate limiting
      (run-at-time (+ 0.1 (* 0.2 (random 10))) nil #'my/slack-backfill-room room team))))

(defun my/slack-smart-backfill-team (team)
  "Intelligently backfill TEAM history.
Prioritizes unread rooms and only updates rooms with new activity compared to DB."
  (when team
    (message "Starting smart backfill for team %s..." (oref team name))
    
    (let* ((team-id (oref team id))
           (all-rooms (my/slack-team-rooms team))
           (db-ts-map (my/slack-get-all-db-latest-ts team-id))
           (unread-rooms (my/slack-get-unread-rooms team))
           (rooms-to-update '())
           (processed-ids (make-hash-table :test 'equal)))

      ;; 1. Queue Unread Rooms (High Priority)
      (dolist (room unread-rooms)
        (when (my/slack-room-active-p room team)
          (puthash (oref room id) t processed-ids)
          (push room rooms-to-update)))

      ;; 2. Check for other updates (Slack Latest > DB Latest)
      (dolist (room all-rooms)
        (let ((id (oref room id)))
          (unless (or (gethash id processed-ids)
                      (not (my/slack-room-active-p room team)))
            (let ((slack-latest (slack-room-latest room team))
                  (db-latest (gethash id db-ts-map)))
              ;; If Slack has data AND (DB is empty OR Slack is newer)
              (when (and (not (equal slack-latest "0"))
                         (or (null db-latest)
                             (my/slack-ts-greater-p slack-latest db-latest)))
                (push room rooms-to-update))))))
      
      (message "Smart Backfill: Found %d unread and %d updated rooms out of %d total."
               (length unread-rooms)
               (- (length rooms-to-update) (length unread-rooms))
               (length all-rooms))

      ;; 3. Execute Backfill (with rate limiting)
      (dolist (room rooms-to-update)
        (run-at-time (+ 0.1 (* 0.2 (random 10))) nil #'my/slack-backfill-room room team)))))

(defun my/slack-sync-current-team ()
  "Manually trigger history backfill for the current team."
  (interactive)
  (if (and (boundp 'slack-current-team) slack-current-team)
      (my/slack-backfill-team slack-current-team)
    (message "No current Slack team selected.")))

;;; Hooks and Advice

(defun my/slack-on-conversations-update (orig-fun &optional team after-success)
  "Advice to run backfill after slack-conversations-list-update completes."
  (funcall orig-fun team 
           (lambda (t-arg)
             (when (functionp after-success)
               (funcall after-success t-arg))
             (my/slack-smart-backfill-team t-arg))))

(defun my/slack-log-incoming-db (payload team)
  "Log incoming Slack PAYLOAD (plist) to DB."
  (let* ((type (plist-get payload :type))
         (subtype (plist-get payload :subtype))
         (user-id (plist-get payload :user))
         (channel-id (plist-get payload :channel))
         (text (plist-get payload :text)))
    (when (and (equal type "message")
               (or (null subtype) (equal subtype "me_message"))
               text
               user-id)
      (let ((room (slack-room-find channel-id team)))
        (when (and room (not (my/slack-room-ignored-p room team)))
          ;; Create a message object so we can use the same insert function
          (let ((msg (slack-message-create payload team room)))
            (my/slack-db-insert-message msg team (oref room id))))))))

(defun my/slack-log-outgoing-db (message room team &rest args)
  "Log outgoing Slack MESSAGE (string) to DB."
  (let ((ts (format-time-string "%s" (time-to-seconds)))
        (user-id (oref team self-id))
        (team-id (oref team id))
        (room-id (oref room id))
        (sender-name (or (oref team self-name) "Me")))
     (let ((db (sqlite-open my/slack-db-path)))
      (sqlite-execute db "INSERT OR IGNORE INTO messages (ts, team_id, room_id, user_id, sender_name, text, thread_ts, json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                      (list ts team-id room-id user-id sender-name message nil ""))
      (sqlite-close db))))

(defun my/setup-slack-db-logging ()
  "Setup hooks and advice for Slack DB logging."
  (interactive)
  (my/slack-db-init)
  
  ;; Backfill when conversations list is updated (happens on connect)
  (advice-add 'slack-conversations-list-update :around #'my/slack-on-conversations-update)
  
  ;; Real-time logging
  (advice-add 'slack-ws-handle-message :before #'my/slack-log-incoming-db)
  (advice-add 'slack-message-send-internal :before #'my/slack-log-outgoing-db)
  (message "Slack DB logging enabled"))

;;; Viewer

(defun my/slack-open-room (team-id room-id)
  "Open the Slack room buffer for TEAM-ID and ROOM-ID."
  (let ((team (slack-team-find team-id)))
    (if team
        (let ((room (slack-room-find room-id team)))
          (if room
              (slack-room-display room team)
            (message "Room %s not found in team %s" room-id (slack-team-name team))))
      (message "Team %s not found (or not connected)" team-id))))

(defun my/slack-show-logs ()
  "View Slack logs from DB."
  (interactive)
  (let ((db (sqlite-open my/slack-db-path))
        (buffer (get-buffer-create "*Slack Logs*")))
    (with-current-buffer buffer
      (org-mode)
      (read-only-mode -1)
      (erase-buffer)
      (insert "* Slack Message Logs (Last 1000)\n\n")
      (let ((rows (sqlite-select db "SELECT datetime(ts, 'unixepoch', 'localtime'), sender_name, text, room_id, team_id FROM messages ORDER BY ts DESC LIMIT 1000")))
        (dolist (row rows)
          (let* ((time (nth 0 row))
                 (sender (nth 1 row))
                 (text (nth 2 row))
                 (room-id (nth 3 row))
                 (team-id (nth 4 row))
                 (team (slack-team-find team-id))
                 (room (and team (slack-room-find room-id team)))
                 (room-label (if room (slack-room-name room team) (format "%s" room-id))))
            (insert (format "** [%s] [[elisp:(my/slack-open-room \"%s\" \"%s\")][%s]] %s: %s\n:PROPERTIES:\n:TEAM: %s\n:ROOM: %s\n:END:\n%s\n\n" 
                            time 
                            team-id room-id room-label
                            sender 
                            (replace-regexp-in-string "\n" " " (substring text 0 (min 50 (length text))))
                            team-id room-id
                            text)))))
      (read-only-mode 1)
      (switch-to-buffer buffer))
    (sqlite-close db)))

(defalias 'my/open-latest-slack-log 'my/slack-show-logs)

(defun my/slack-log-message-at-point ()
  "Log the message at point in the current Slack buffer to the SQLite DB."
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buffer slack-current-buffer)
                  (team (slack-buffer-team buffer))
                  (room (slack-buffer-room buffer))
                  (message (slack-room-find-message room ts)))
    (let* ((room-id (oref room id)))
      (my/slack-db-insert-message message team room-id)
      (message "Message logged to DB."))))

(provide 'my-slack-db)
