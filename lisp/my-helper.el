;;; my-helper.el --- Generic helper functions -*- lexical-binding: t; -*-

(defun my/flash-mode-line ()
  "Flash the mode line briefly."
  (let ((orig-bg (face-background 'mode-line)))
    (set-face-background 'mode-line "#F2804F")
    (run-with-idle-timer 0.1 nil
                         (lambda (bg)
                           (set-face-background 'mode-line bg))
                         orig-bg)))

(defun my/open-in-external-app ()
  "Open the current file or dired-marked file in an external app."
  (interactive)
  (let ((file (cond
               ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
               (t buffer-file-name))))
    (if (and file (not (string-empty-p file)))
        (progn
          (start-process "open-external" nil "open" file)
          (message "Opened in external app: %s" file))
      (message "Could not determine file path."))))

(defun my/open-user-readme ()
  "Open the README from `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "readme.md" user-emacs-directory)))

(provide 'my-helper)
;;; my-helper.el ends here
