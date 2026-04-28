;;; my-slack-db.el --- Slack logging to SQLite -*- lexical-binding: t; -*-

(require 'sqlite)
(require 'cl-lib)
(require 'slack)
(require 'slack-message)
(require 'slack-conversations)

(defvar my/slack-db-path (expand-file-name "var/slack-messages.db" user-emacs-directory))

(defvar my/slack-active-rooms-cache (make-hash-table :test 'equal)
  "In-memory cache of room-id -> is_active (boolean).")

(defun my/slack-db-init ()
  "Initialize the Slack SQLite database."
  (unless (file-exists-p (file-name-directory my/slack-db-path))
    (make-directory (file-name-directory my/slack-db-path) t))
  (let ((db (sqlite-open my/slack-db-path)))
    ;; Messages Table
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
    ;; Channels Table
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS channels (
        team_id TEXT,
        room_id TEXT,
        name TEXT,
        type_id INTEGER,
        is_active INTEGER DEFAULT 1,
        is_member INTEGER DEFAULT 0,
        date_created TEXT,
        PRIMARY KEY (team_id, room_id)
      )")
    ;; Migration: Add missing columns if they don't exist
    (condition-case nil
        (sqlite-execute db "ALTER TABLE channels ADD COLUMN date_created TEXT")
      (error nil))
    (condition-case nil
        (sqlite-execute db "ALTER TABLE channels ADD COLUMN is_member INTEGER DEFAULT 0")
      (error nil))
    (condition-case nil
        (sqlite-execute db "ALTER TABLE channels ADD COLUMN type_id INTEGER")
      (error nil))
    (sqlite-close db)))

(defun my/slack-db-sync-channels (team)
  "Sync discovered rooms for TEAM into the database channels table."
  (let ((db (sqlite-open my/slack-db-path))
        (team-id (oref team id))
        ;; Categorize rooms by type for easy type_id assignment
        (channel-list (mapcar (lambda (r) (list r 1)) (hash-table-values (oref team channels))))
        (group-list (mapcar (lambda (r) (list r 2)) (hash-table-values (oref team groups))))
        (im-list (mapcar (lambda (r) (list r 3)) (hash-table-values (oref team ims)))))
    (dolist (item (append channel-list group-list im-list))
      (let* ((room (nth 0 item))
             (type-id (nth 1 item))
             (room-id (oref room id))
             (name (slack-room-name room team))
             (is-member (if (slack-room-member-p room) 1 0))
             ;; slack-room objects have a 'created' field (Unix timestamp)
             (created (if (slot-boundp room 'created) (oref room created) nil))
             (created-date (if created 
                               (format-time-string "%Y-%m-%d" (seconds-to-time created))
                             "Unknown")))
        ;; Insert if not exists, otherwise update name, membership, type, and created date
        (sqlite-execute db "
          INSERT INTO channels (team_id, room_id, name, type_id, is_active, is_member, date_created)
          VALUES (?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT(team_id, room_id) DO UPDATE SET 
            name = excluded.name, 
            type_id = excluded.type_id,
            is_member = excluded.is_member,
            date_created = excluded.date_created"
                        (list team-id room-id name type-id (if (= is-member 1) 1 0) is-member created-date))))
    (sqlite-close db)
    ;; Refresh the local cache
    (my/slack-db-refresh-active-cache team-id)))

(defun my/slack-db-refresh-active-cache (team-id)
  "Load the active status for all rooms in TEAM-ID into the cache."
  (let ((db (sqlite-open my/slack-db-path)))
    (clrhash my/slack-active-rooms-cache)
    (let ((rows (sqlite-select db "SELECT room_id, is_active FROM channels WHERE team_id = ?" (list team-id))))
      (dolist (row rows)
        (puthash (nth 0 row) (= (nth 1 row) 1) my/slack-active-rooms-cache)))
    (sqlite-close db)))

(defun my/slack-room-ignored-p (room team)
  "Return t if ROOM in TEAM should be ignored based on DB status."
  (let* ((room-id (oref room id))
         (active (gethash room-id my/slack-active-rooms-cache 'not-found)))
    (if (eq active 'not-found)
        nil ;; Assume active if not in cache (safety fallback)
      (not active))))

(defun my/slack-room-active-p (room team)
  "Return t if ROOM is active (marked for reading in DB, not archived, open, and member)."
  (and (not (my/slack-room-ignored-p room team))
       (not (slack-room-archived-p room))
       (slack-room-open-p room)
       (slack-room-member-p room)))

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
    
    ;; Sync channels first to discover new ones and update local cache
    (my/slack-db-sync-channels team)
    
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
      (my/slack-smart-backfill-team slack-current-team)
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
      ;; Join with channels table to get the type_id (1=Chan, 2=Group, 3=DM)
      (let ((rows (sqlite-select db "
        SELECT datetime(m.ts, 'unixepoch', 'localtime'), m.sender_name, m.text, m.room_id, m.team_id, c.type_id, date(m.ts, 'unixepoch', 'localtime')
        FROM messages m 
        LEFT JOIN channels c ON m.team_id = c.team_id AND m.room_id = c.room_id 
        ORDER BY m.ts DESC LIMIT 100"))
            (current-date nil))
        (dolist (row rows)
          (let* ((datetime (nth 0 row))
                 (sender (nth 1 row))
                 (text (nth 2 row))
                 (room-id (nth 3 row))
                 (team-id (nth 4 row))
                 (type-id (nth 5 row))
                 (msg-date (nth 6 row))
                 (is-dm (= (or type-id 0) 3))
                 (team (slack-team-find team-id))
                 (room (and team (slack-room-find room-id team)))
                 (room-label (if room (slack-room-name room team) (format "%s" room-id)))
                 ;; Styles
                 (sender-face (if is-dm 'font-lock-function-name-face 'font-lock-keyword-face))
                 (prefix (if is-dm 
                             (propertize "[DM]" 'face 'font-lock-warning-face)
                           (propertize "[#]" 'face 'font-lock-comment-face))))
            
            ;; Insert day divider
            (when (and current-date (not (string= msg-date current-date)))
              (insert (propertize "\n" 'face 'default)
                      (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n" 'face 'shadow)
                      (propertize (format "  --- %s ---\n" current-date) 'face 'font-lock-comment-face)
                      (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n" 'face 'shadow)))
            (setq current-date msg-date)

            (insert (format "** %s %s [[slack:%s:%s][%s]]\n%s: %s\n\n" 
                            prefix
                            datetime 
                            team-id room-id room-label
                            (propertize sender 'face sender-face)
                            (propertize text 'face (if is-dm 'italic 'default)))))))
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

;;; Channel Management UI

(defvar-local my/slack-current-team nil)

(define-derived-mode my/slack-channel-manager-mode tabulated-list-mode "SlackChannels"
  "Major mode for managing Slack channel read/ignore status."
  (setq tabulated-list-format [("Status" 10 t)
                               ("Member" 8 t)
                               ("Type" 8 t)
                               ("Name" 25 t)
                               ("Created Date" 12 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun my/slack-manage-channels ()
  "Show the Slack channel management buffer."
  (interactive)
  (unless (and (boundp 'slack-current-team) slack-current-team)
    (error "No current Slack team. Connect to Slack first"))
  ;; Ensure DB and tables are initialized
  (my/slack-db-init)
  (let* ((team slack-current-team)
         (buffer (get-buffer-create "*Slack Channels*")))
    (with-current-buffer buffer
      (my/slack-channel-manager-mode)
      (setq-local my/slack-current-team team)
      ;; Refresh the DB list from API first to catch new ones
      (my/slack-db-sync-channels team)
      (my/slack-channel-manager-refresh)
      (local-set-key (kbd "t") #'my/slack-channel-manager-toggle)
      (local-set-key (kbd "RET") #'my/slack-channel-manager-open)
      (local-set-key (kbd "g") #'my/slack-channel-manager-refresh))
    (switch-to-buffer buffer)))

(defun my/slack-channel-manager-refresh ()
  "Refresh the tabulated list entries."
  (interactive)
  (let* ((team-id (oref my/slack-current-team id))
         (db (sqlite-open my/slack-db-path))
         (rows (sqlite-select db "SELECT room_id, name, is_active, is_member, date_created, type_id FROM channels WHERE team_id = ? ORDER BY is_member DESC, type_id ASC, is_active DESC, date_created DESC, name ASC" (list team-id))))
    (sqlite-close db)
    (setq tabulated-list-entries
          (mapcar (lambda (row)
                    (let* ((room-id (nth 0 row))
                           (name (nth 1 row))
                           (is-active (= (nth 2 row) 1))
                           (is-member (= (nth 3 row) 1))
                           (date-created (nth 4 row))
                           (type-id (nth 5 row))
                           (status-text (if is-active "[READ]" "[IGNORE]"))
                           (status-face (if is-active 'success 'shadow))
                           (member-text (if is-member "Yes" "No"))
                           (member-face (if is-member 'font-lock-keyword-face 'shadow))
                           (type-text (cond ((= (or type-id 0) 1) "Chan")
                                            ((= (or type-id 0) 2) "Group")
                                            ((= (or type-id 0) 3) "DM")
                                            (t "Unknown"))))
                      (list room-id 
                            (vector (propertize status-text 'face status-face)
                                    (propertize member-text 'face member-face)
                                    type-text
                                    name
                                    (or date-created "")))))
                  rows))
    (tabulated-list-print t)))

(defun my/slack-channel-manager-toggle ()
  "Toggle the read/ignore status of the channel at point."
  (interactive)
  (let* ((room-id (tabulated-list-get-id))
         (team-id (oref my/slack-current-team id)))
    (when room-id
      (let ((db (sqlite-open my/slack-db-path)))
        (sqlite-execute db "UPDATE channels SET is_active = 1 - is_active WHERE team_id = ? AND room_id = ?" (list team-id room-id))
        (sqlite-close db)
        ;; Update the cache and refresh the view
        (my/slack-db-refresh-active-cache team-id)
        (my/slack-channel-manager-refresh)))))

(defun my/slack-channel-manager-open ()
  "Open the channel at point."
  (interactive)
  (let* ((room-id (tabulated-list-get-id))
         (team my/slack-current-team))
    (when room-id
      (my/slack-open-room (oref team id) room-id))))

(provide 'my-slack-db)
