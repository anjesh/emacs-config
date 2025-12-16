;;; qwen-cli.el --- Qwen CLI Emacs integration -*- lexical-binding: t; -*-

;; Author: Lin Chen<lc1990linux@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (transient "0.9.3"))

;; URL: https://github.com/linchen2chris/qwen-cli.el

;;; Commentary:
;; An Emacs interface to Qwen CLI.  This package provides convenient
;; ways to interact with Qwen from within Emacs, including sending
;; commands, toggling the Qwen window, and accessing slash commands.

;;; Code:

(require 'transient)
(require 'project)
(require 'cl-lib)
(require 'popup)
(require 'projectile)

;;;; Customization options
(defgroup qwen-cli nil
  "Qwen AI interface for Emacs."
  :group 'tools)

(defgroup qwen-cli-eat nil
  "Eat terminal backend specific settings for Qwen CLI."
  :group 'qwen-cli)

(defgroup qwen-cli-vterm nil
  "Vterm terminal backend specific settings for Qwen CLI."
  :group 'qwen-cli)

(defgroup qwen-cli-window nil
  "Window management settings for Qwen CLI."
  :group 'qwen-cli)

(defface qwen-cli-repl-face
  nil
  "Face for Qwen REPL."
  :group 'qwen-cli)

(defcustom qwen-cli-term-name "xterm-256color"
  "Terminal type to use for Qwen REPL."
  :type 'string
  :group 'qwen-cli)

(defcustom qwen-cli-start-hook nil
  "Hook run after Qwen is started."
  :type 'hook
  :group 'qwen-cli)

(defcustom qwen-cli-slash-commands
  '(
    "/about"
    "/auth"
    "/bug"
    ("/chat" "/chat list" "/chat save" "/chat resume")
    "/clear"
    "/compress"
    "/copy"
    "/docs"
    ("/directory" "/directory add" "/directory show")
    "/editor"
    "/extensions"
    "/help"
    "/ide"
    "/init"
    ("/mcp" "/mcp list" "/mcp auth" "/mcp refresh")
    ("/memory" "/memory show""/memory add" "/memory refresh")
    "/privacy"
    "/quit"
    ("/stats" "/stats model" "/stats tools")
    "/theme"
    "/tools"
    "/settings"
    "/vim"
    "/setup-github"
    "/terminal-setup")
  "List of slash commands available in Qwen."
:type '(repeat (choice string (repeat string)))
:group 'qwen-cli)

(defcustom qwen-cli-startup-delay 0.1
  "Delay in seconds after starting Qwen before displaying buffer.

This helps fix terminal layout issues that can occur if the buffer
is displayed before Qwen is fully initialized."
  :type 'number
  :group 'qwen-cli)

(defcustom qwen-cli-large-buffer-threshold 100000
  "Size threshold in characters above which buffers are considered \"large\".

When sending a buffer to Qwen with `qwen-cli-send-region` and no
region is active, prompt for confirmation if buffer size exceeds this value."
  :type 'integer
  :group 'qwen-cli)

(defcustom qwen-cli-program "qwen"
  "Program to run when starting Qwen.
This is passed as the PROGRAM parameter to `eat-make`."
  :type 'string
  :group 'qwen-cli)

(defcustom qwen-cli-program-switches nil
  "List of command line switches to pass to the Qwen program.
These are passed as SWITCHES parameters to `eat-make`."
  :type '(repeat string)
  :group 'qwen-cli)

(defcustom qwen-cli-newline-keybinding-style 'newline-on-shift-return
  "Key binding style for entering newlines and sending messages.

This controls how the return key and its modifiers behave in Qwen buffers:
- \\='newline-on-shift-return: S-return enters a line break, RET sends the
  command (default)
- \\='newline-on-alt-return: M-return enters a line break, RET sends the command
- \\='shift-return-to-send: RET enters a line break, S-return sends the command
- \\='super-return-to-send: RET enters a line break, s-return sends the command

`\"S\"' is the shift key.
`\"s\"' is the hyper key, which is the COMMAND key on macOS."
  :type '(choice (const :tag "Newline on shift-return (s-return for newline, RET to send)" newline-on-shift-return)
                 (const :tag "Newline on alt-return (M-return for newline, RET to send)" newline-on-alt-return)
                 (const :tag "Shift-return to send (RET for newline, S-return to send)" shift-return-to-send)
                 (const :tag "Super-return to send (RET for newline, s-return to send)" super-return-to-send))
  :group 'qwen-cli)

(defcustom qwen-cli-enable-notifications t
  "Whether to show notifications when Qwen finishes and awaits input."
  :type 'boolean
  :group 'qwen-cli)

(defcustom qwen-cli-notification-function 'qwen-cli-default-notification
  "Function to call for notifications.

The function is called with two arguments:
- TITLE: Title of the notification
- MESSAGE: Body of the notification

You can set this to your own custom notification function.
The default function displays a message and pulses the modeline
to provide visual feedback when Qwen is ready for input."
  :type 'function
  :group 'qwen-cli)

(defcustom qwen-cli-confirm-kill t
  "Whether to ask for confirmation before killing Qwen instances.

When non-nil, qwen-cli-kill will prompt for confirmation.
When nil, Qwen instances will be killed without confirmation."
  :type 'boolean
  :group 'qwen-cli)

(defcustom qwen-cli-optimize-window-resize t
  "Whether to optimize terminal window resizing to prevent unnecessary reflows.

When non-nil, terminal reflows are only triggered when the window width
changes, not when only the height changes. This prevents unnecessary
terminal redraws when windows are split or resized vertically, improving
performance and reducing visual artifacts.

Set to nil if you experience issues with terminal display after window
resizing."
  :type 'boolean
  :group 'qwen-cli)

(defcustom qwen-cli-no-delete-other-windows nil
  "Whether to prevent Qwen CLI windows from being deleted.

When non-nil, qwen-cli will have the `no-delete-other-windows'
parameter.  This parameter prevents the qwen-cli window from
closing when calling `delete-other-windows' or any command that would
launch a new full-screen buffer."
  :type 'boolean
  :group 'qwen-cli-window)

;;;;; Eat terminal customizations
;; Eat-specific terminal faces
(defface qwen-cli-eat-prompt-annotation-running-face
  '((t :inherit eat-shell-prompt-annotation-running))
  "Face for running prompt annotations in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-prompt-annotation-success-face
  '((t :inherit eat-shell-prompt-annotation-success))
  "Face for successful prompt annotations in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-prompt-annotation-failure-face
  '((t :inherit eat-shell-prompt-annotation-failure))
  "Face for failed prompt annotations in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-term-bold-face
  '((t :inherit eat-term-bold))
  "Face for bold text in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-term-faint-face
  '((t :inherit eat-term-faint))
  "Face for faint text in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-term-italic-face
  '((t :inherit eat-term-italic))
  "Face for italic text in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-term-slow-blink-face
  '((t :inherit eat-term-slow-blink))
  "Face for slow blinking text in Qwen eat terminal."
  :group 'qwen-cli-eat)

(defface qwen-cli-eat-term-fast-blink-face
  '((t :inherit eat-term-fast-blink))
  "Face for fast blinking text in Qwen eat terminal."
  :group 'qwen-cli-eat)

(dotimes (i 10)
  (let ((face-name (intern (format "qwen-cli-eat-term-font-%d-face" i)))
        (eat-face (intern (format "eat-term-font-%d" i))))
    (eval `(defface ,face-name
             '((t :inherit ,eat-face))
             ,(format "Face for font %d in Qwen eat terminal." i)
             :group 'qwen-cli-eat))))

(defcustom qwen-cli-eat-read-only-mode-cursor-type '(box nil nil)
  "Type of cursor to use as invisible cursor in Qwen CLI terminal buffer.

The value is a list of form (CURSOR-ON BLINKING-FREQUENCY CURSOR-OFF).

When the cursor is on, CURSOR-ON is used as `cursor-type', which see.
BLINKING-FREQUENCY is the blinking frequency of cursor's blinking.
When the cursor is off, CURSOR-OFF is used as `cursor-type'.  This
should be nil when cursor is not blinking.

Valid cursor types for CURSOR-ON and CURSOR-OFF:
- t: Frame default cursor
- box: Filled box cursor
- (box . N): Box cursor with specified size N
- hollow: Hollow cursor
- bar: Vertical bar cursor
- (bar . N): Vertical bar with specified height N
- hbar: Horizontal bar cursor
- (hbar . N): Horizontal bar with specified width N
- nil: No cursor

BLINKING-FREQUENCY can be nil (no blinking) or a number."
  :type '(list
          (choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None" nil))
          (choice
           (const :tag "No blinking" nil)
           (number :tag "Blinking frequency"))
          (choice
           (const :tag "Frame default" t)
           (const :tag "Filled box" box)
           (cons :tag "Box with specified size" (const box) integer)
           (const :tag "Hollow cursor" hollow)
           (const :tag "Vertical bar" bar)
           (cons :tag "Vertical bar with specified height" (const bar)
                 integer)
           (const :tag "Horizontal bar" hbar)
           (cons :tag "Horizontal bar with specified width"
                 (const hbar) integer)
           (const :tag "None" nil)))
  :group 'qwen-cli-eat)

(defcustom qwen-cli-eat-never-truncate-qwen-buffer nil
  "When non-nil, disable truncation of Qwen output buffer.

By default, Eat will truncate the terminal scrollback buffer when it
reaches a certain size.  This can cause Qwen's output to be cut off
when dealing with large responses.  Setting this to non-nil disables
the scrollback size limit, allowing Qwen to output unlimited content
without truncation.

Note: Disabling truncation may consume more memory for very large
outputs."
  :type 'boolean
  :group 'qwen-cli-eat)

(make-obsolete-variable 'qwen-cli-eat-never-truncate-qwen-buffer
                        "Setting it to t can consume more memory for very large outputs and can cause performance issues with long Qwen sessions"
                        "0.4.0")

;;;;; Vterm terminal customizations
(defcustom qwen-cli-vterm-buffer-multiline-output t
  "Whether to buffer vterm output to prevent flickering on multi-line input.

When non-nil, vterm output that appears to be redrawing multi-line
input boxes will be buffered briefly and processed in a single
batch. This prevents the flickering that can occur when Qwen redraws
its input box as it expands to multiple lines.

This only affects the vterm backend."
  :type 'boolean
  :group 'qwen-cli-vterm)

(defcustom qwen-cli-vterm-multiline-delay 0.01
  "Delay in seconds before processing buffered vterm output.

This controls how long vterm waits to collect output before processing
it when `qwen-cli-vterm-buffer-multiline-output' is enabled.
The delay should be long enough to collect bursts of updates but short
enough to not be noticeable to the user.

The default value of 0.01 seconds (10ms) provides a good balance
between reducing flickering and maintaining responsiveness."
  :type 'number
  :group 'qwen-cli-vterm)

;;;; Forward declrations for flycheck
(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-filename "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")

;;;; Internal state variables
(defvar qwen-cli--directory-buffer-map (make-hash-table :test 'equal)
  "Hash table mapping directories to user-selected Qwen buffers.
Keys are directory paths, values are buffer objects.
This allows remembering which Qwen instance the user selected
for each directory across multiple invocations.")

(defvar qwen-cli--window-widths nil
  "Hash table mapping windows to their last known widths for eat terminals.")

;;;; Key bindings
;;;###autoload (autoload 'qwen-cli-command-map "qwen-cli")
(defvar qwen-cli-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") 'qwen-cli-slash-commands-popup)
    (define-key map (kbd "!") 'qwen-cli-send-shell)
    (define-key map (kbd "a") 'qwen-cli-add-context)
    (define-key map (kbd "b") 'qwen-cli-switch-to-buffer)
    (define-key map (kbd "B") 'qwen-cli-select-buffer)
    (define-key map (kbd "c") 'qwen-cli)
    (define-key map (kbd "C") 'qwen-cli-continue)
    (define-key map (kbd "RET") 'qwen-cli-quick-response)
    (define-key map (kbd "R") 'qwen-cli-resume)
    (define-key map (kbd "i") 'qwen-cli-new-instance)
    (define-key map (kbd "d") 'qwen-cli-start-in-directory)
    (define-key map (kbd "e") 'qwen-cli-fix-error-at-point)
    (define-key map (kbd "k") 'qwen-cli-kill)
    (define-key map (kbd "K") 'qwen-cli-kill-all)
    (define-key map (kbd "m") 'qwen-cli-transient)
    (define-key map (kbd "n") 'qwen-cli-send-escape)
    (define-key map (kbd "f") 'qwen-cli-fork)
    (define-key map (kbd "r") 'qwen-cli-send-region)
    (define-key map (kbd "s") 'qwen-cli-send-command)
    (define-key map (kbd "t") 'qwen-cli-toggle)
    (define-key map (kbd "x") 'qwen-cli-send-command-with-context)
    (define-key map (kbd "y") 'qwen-cli-send-return)
    (define-key map (kbd "z") 'qwen-cli-toggle-read-only-mode)
    (define-key map (kbd "1") 'qwen-cli-send-1)
    (define-key map (kbd "2") 'qwen-cli-send-2)
    (define-key map (kbd "3") 'qwen-cli-send-3)
    (define-key map (kbd "4") 'qwen-cli-send-4)
    (define-key map (kbd "M") 'qwen-cli-cycle-mode)
    (define-key map (kbd "o") 'qwen-cli-send-buffer-file)
    map)
  "Keymap for Qwen commands.")

;;;; Transient Menus
;;;###autoload (autoload 'qwen-cli-transient "qwen-cli" nil t)
(transient-define-prefix qwen-cli-transient ()
  "Qwen command menu."
  ["Qwen Commands"
   ["Start/Stop Qwen"
    ("c" "Start Qwen" qwen-cli)
    ("d" "Start in directory" qwen-cli-start-in-directory)
    ("C" "Continue conversation" qwen-cli-continue)
    ("R" "Resume session" qwen-cli-resume)
    ("i" "New instance" qwen-cli-new-instance)
    ("k" "Kill Qwen" qwen-cli-kill)
    ("K" "Kill all Qwen instances" qwen-cli-kill-all)
    ]
   ["Send Commands to Qwen"
    ("s" "Send command" qwen-cli-send-command)
    ("x" "Send command with context" qwen-cli-send-command-with-context)
    ("r" "Send region or buffer" qwen-cli-send-region)
    ("o" "Send buffer file" qwen-cli-send-buffer-file)
    ("e" "Fix error at point" qwen-cli-fix-error-at-point)
    ("f" "Fork conversation" qwen-cli-fork)
    ("/" "Slash Commands" qwen-cli-slash-commands-popup)]
   ["Manage Qwen"
    ("t" "Toggle qwen window" qwen-cli-toggle)
    ("b" "Switch to Qwen buffer" qwen-cli-switch-to-buffer)
    ("B" "Select from all Qwen buffers" qwen-cli-select-buffer)
    ("z" "Toggle read-only mode" qwen-cli-toggle-read-only-mode)
    ("M" "Cycle Qwen mode" qwen-cli-cycle-mode :transient t)
    ]
   ["Quick Responses"
    ("1" "Send \"1\"" qwen-cli-send-1)
    ("2" "Send \"2\"" qwen-cli-send-2)
    ("3" "Send \"3\"" qwen-cli-send-3)
    ("4" "Send \"4\"" qwen-cli-send-4)
    ]])

;;;; add files to context
(defun qwen-cli-add-context ()
  "Add FILE to Qwen context."
        (interactive)
        (let ((file (projectile-completing-read "Add file to Qwen: "
                                               (projectile-project-files (projectile-acquire-root)))))
        (qwen-cli--do-send-command (concat "@" file))))

(defun qwen-cli-quick-response ()
  "Send a quick response to Qwen."
(interactive)
(let ((response (read-number "choose(1, 2, 3 or 4):" 1)))
(cond
 ((equal response 1)
  (qwen-cli-send-1))
 ((equal response 2)
  (qwen-cli-send-2))
 ((equal response 3)
  (qwen-cli-send-3))
 ((equal response 4)
  (qwen-cli-send-4))
 (t
  (message "Unknown response: %s" response)))))

;;;; Slash Commands with popup menu
(defun qwen-cli-slash-commands-popup ()
  "Display the Qwen slash commands menu."
  (interactive)
  (let ((slash-cmd (popup-cascade-menu  qwen-cli-slash-commands)))
  (qwen-cli--do-send-command slash-cmd)))

;;;; Terminal abstraction layer
;; This layer abstracts terminal operations to support multiple backends (eat, vterm, etc.)

(require 'cl-lib)

(defcustom qwen-cli-terminal-backend 'eat
  "Terminal backend to use for Qwen CLI.
Choose between \\='eat (default) and \\='vterm terminal emulators."
  :type '(radio (const :tag "Eat terminal emulator" eat)
                (const :tag "Vterm terminal emulator" vterm))
  :group 'qwen-cli)

;;;;; Generic function definitions

(cl-defgeneric qwen-cli--term-make (backend buffer-name program &optional switches)
  "Create a terminal using BACKEND in BUFFER-NAME running PROGRAM.
Optional SWITCHES are command-line arguments to PROGRAM.
Returns the buffer containing the terminal.")

(cl-defgeneric qwen-cli--term-send-string (backend terminal string)
  "Send STRING to TERMINAL using BACKEND.")

(cl-defgeneric qwen-cli--term-kill-process (backend buffer)
  "Kill the terminal process in BUFFER using BACKEND.")

(cl-defgeneric qwen-cli--term-read-only-mode (backend)
  "Switch current terminal to read-only mode using BACKEND.")

(cl-defgeneric qwen-cli--term-interactive-mode (backend)
  "Switch current terminal to interactive mode using BACKEND.")

(cl-defgeneric qwen-cli--term-in-read-only-p (backend)
  "Check if current terminal is in read-only mode using BACKEND.")

(cl-defgeneric qwen-cli--term-configure (backend)
  "Configure terminal in current buffer with BACKEND specific settings.")

(cl-defgeneric qwen-cli--term-customize-faces (backend)
  "Apply face customizations for the terminal using BACKEND.")

(cl-defgeneric qwen-cli--term-setup-keymap (backend)
  "Set up the local keymap for Qwen CLI buffers using BACKEND.")

(cl-defgeneric qwen-cli--term-get-adjust-process-window-size-fn (backend)
  "Get the BACKEND specific function that adjusts window size.")

;;;;; eat backend implementations

;; Declare external variables and functions from eat package
(defvar eat--semi-char-mode)
(defvar eat--synchronize-scroll-function)
(defvar eat-invisible-cursor-type)
(defvar eat-term-name)
(defvar eat-terminal)
(declare-function eat--adjust-process-window-size "eat" (&rest args))
(declare-function eat--set-cursor "eat" (terminal &rest args))
(declare-function eat-emacs-mode "eat")
(declare-function eat-kill-process "eat" (&optional buffer))
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat-semi-char-mode "eat")
(declare-function eat-term-display-beginning "eat" (terminal))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-live-p "eat" (terminal))
(declare-function eat-term-parameter "eat" (terminal parameter) t)
(declare-function eat-term-redisplay "eat" (terminal))
(declare-function eat-term-reset "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))

;; Helper to ensure eat is loaded
(defun qwen-cli--ensure-eat ()
  "Ensure eat package is loaded."
  (unless (featurep 'eat)
    (unless (require 'eat nil t)
      (error "The eat package is required for eat terminal backend. Please install it"))))

(cl-defmethod qwen-cli--term-make ((_backend (eql eat)) buffer-name program &optional switches)
  "Create an eat terminal for BACKEND.

_BACKEND is the terminal backend type (should be \\='eat).
BUFFER-NAME is the name for the new terminal buffer.
PROGRAM is the program to run in the terminal.
SWITCHES are optional command-line arguments for PROGRAM."
  (qwen-cli--ensure-eat)

  (let* ((trimmed-buffer-name (string-trim-right (string-trim buffer-name "\\*") "\\*")))
    (apply #'eat-make trimmed-buffer-name program nil switches)))

(cl-defmethod qwen-cli--term-send-string ((_backend (eql eat)) string)
  "Send STRING to eat terminal.

_BACKEND is the terminal backend type (should be \\='eat).
STRING is the text to send to the terminal."
  (eat-term-send-string eat-terminal string))

(cl-defmethod qwen-cli--term-kill-process ((_backend (eql eat)) buffer)
  "Kill the eat terminal process in BUFFER.

_BACKEND is the terminal backend type (should be \\='eat).
BUFFER is the terminal buffer containing the process to kill."
  (with-current-buffer buffer
    (eat-kill-process)
    (kill-buffer buffer)))

(cl-defmethod qwen-cli--term-read-only-mode ((_backend (eql eat)))
  "Switch eat terminal to read-only mode.

_BACKEND is the terminal backend type (should be \\'eat)."
  (qwen-cli--ensure-eat)
  (eat-emacs-mode)
  (setq-local eat-invisible-cursor-type qwen-cli-eat-read-only-mode-cursor-type)
  (eat--set-cursor nil :invisible))

(cl-defmethod qwen-cli--term-interactive-mode ((_backend (eql eat)))
  "Switch eat terminal to interactive mode.

_BACKEND is the terminal backend type (should be \\='eat)."
  (qwen-cli--ensure-eat)
  (eat-semi-char-mode)
  (setq-local eat-invisible-cursor-type nil)
  (eat--set-cursor nil :invisible))

(cl-defmethod qwen-cli--term-in-read-only-p ((_backend (eql eat)))
  "Check if eat terminal is in read-only mode.

_BACKEND is the terminal backend type (should be \\='eat)."
  (not eat--semi-char-mode))

(defun qwen-cli--eat-synchronize-scroll (windows)
  "Synchronize scrolling and point between terminal and WINDOWS.

WINDOWS is a list of windows.  WINDOWS may also contain the special
symbol `buffer', in which case the point of current buffer is set.

This custom version keeps the prompt at the bottom of the window when
possible, preventing the scrolling up issue when editing other buffers."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      ;; Don't move the cursor around when in eat-emacs-mode
      (when (not buffer-read-only)
        (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
          ;; Always set point to cursor position
          (set-window-point window cursor-pos)
          ;; Try to keep cursor visible with minimal scrolling
          (cond
           ;; If cursor is at/near end, keep at bottom
           ((>= cursor-pos (- (point-max) 2))
            (with-selected-window window
              (goto-char cursor-pos)
              (recenter -1)))
           ;; If cursor not visible, scroll minimally to show it
           ((not (pos-visible-in-window-p cursor-pos window))
            (with-selected-window window
              (goto-char cursor-pos)
              ;; Center cursor in window instead of jumping to term beginning
              (recenter)))))))))

(cl-defmethod qwen-cli--term-configure ((_backend (eql eat)))
  "Configure eat terminal in current buffer.

_BACKEND is the terminal backend type (should be \\='eat)."
  (qwen-cli--ensure-eat)
  ;; Configure eat-specific settings
  (setq-local eat-term-name qwen-cli-term-name)
  (setq-local eat-enable-directory-tracking nil)
  (setq-local eat-enable-shell-command-history nil)
  (setq-local eat-enable-shell-prompt-annotation nil)
  (when qwen-cli-eat-never-truncate-qwen-buffer
    (setq-local eat-term-scrollback-size nil))

  ;; Set up custom scroll function to stop eat from scrolling to the top
  (setq-local eat--synchronize-scroll-function #'qwen-cli--eat-synchronize-scroll)

  ;; Configure bell handler - ensure eat-terminal exists
  (when (bound-and-true-p eat-terminal)
    (eval '(setf (eat-term-parameter eat-terminal 'ring-bell-function) #'qwen-cli--notify)))

  ;; fix wonky initial terminal layout that happens sometimes if we show the buffer before qwen is ready
  (sleep-for qwen-cli-startup-delay))

(cl-defmethod qwen-cli--term-customize-faces ((_backend (eql eat)))
  "Apply face customizations for eat terminal.

_BACKEND is the terminal backend type (should be \\='eat)."
  ;; Remap eat faces to Qwen-specific faces
  (face-remap-add-relative 'eat-shell-prompt-annotation-running 'qwen-cli-eat-prompt-annotation-running-face)
  (face-remap-add-relative 'eat-shell-prompt-annotation-success 'qwen-cli-eat-prompt-annotation-success-face)
  (face-remap-add-relative 'eat-shell-prompt-annotation-failure 'qwen-cli-eat-prompt-annotation-failure-face)
  (face-remap-add-relative 'eat-term-bold 'qwen-cli-eat-term-bold-face)
  (face-remap-add-relative 'eat-term-faint 'qwen-cli-eat-term-faint-face)
  (face-remap-add-relative 'eat-term-italic 'qwen-cli-eat-term-italic-face)
  (face-remap-add-relative 'eat-term-slow-blink 'qwen-cli-eat-term-slow-blink-face)
  (face-remap-add-relative 'eat-term-fast-blink 'qwen-cli-eat-term-fast-blink-face)
  (dolist (i (number-sequence 0 9))
    (let ((eat-face (intern (format "eat-term-font-%d" i)))
          (qwen-face (intern (format "qwen-cli-eat-term-font-%d-face" i))))
      (face-remap-add-relative eat-face qwen-face))))

(cl-defmethod qwen-cli--term-setup-keymap ((_backend (eql eat)))
  "Set up the local keymap for Qwen CLI buffers.

_BACKEND is the terminal backend type (should be \\='eat)."
  (let ((map (make-sparse-keymap)))
    ;; Inherit parent eat keymap
    (set-keymap-parent map (current-local-map))

    ;; C-g for escape
    (define-key map (kbd "C-g") #'qwen-cli-send-escape)

    ;; Configure key bindings based on user preference
    (pcase qwen-cli-newline-keybinding-style
      ('newline-on-shift-return
       ;; S-return enters a line break, RET sends the command
       (define-key map (kbd "<S-return>") #'qwen-cli--eat-send-alt-return)
       (define-key map (kbd "<return>") #'qwen-cli--eat-send-return))
      ('newline-on-alt-return
       ;; M-return enters a line break, RET sends the command
       (define-key map (kbd "<M-return>") #'qwen-cli--eat-send-alt-return)
       (define-key map (kbd "<return>") #'qwen-cli--eat-send-return))
      ('shift-return-to-send
       ;; RET enters a line break, S-return sends the command
       (define-key map (kbd "<return>") #'qwen-cli--eat-send-alt-return)
       (define-key map (kbd "<S-return>") #'qwen-cli--eat-send-return))
      ('super-return-to-send
       ;; RET enters a line break, s-return sends the command.
       (define-key map (kbd "<return>") #'qwen-cli--eat-send-alt-return)
       (define-key map (kbd "<s-return>") #'qwen-cli--eat-send-return)))
    (use-local-map map)))

(defun qwen-cli--eat-send-alt-return ()
  "Send <alt>-<return> to eat."
  (interactive)
  (eat-term-send-string eat-terminal "\e\C-m"))

(defun qwen-cli--eat-send-return ()
  "Send <return> to eat."
  (interactive)
  (eat-term-send-string eat-terminal (kbd "RET")))

(cl-defgeneric qwen-cli--term-get-adjust-process-window-size-fn (backend)
  "Get the BACKEND specific function that adjusts window size.")

(cl-defmethod qwen-cli--term-get-adjust-process-window-size-fn ((_backend (eql eat)))
  "Get the BACKEND specific function that adjusts window size."
  #'eat--adjust-process-window-size)

;;;;; vterm backend implementations

;; Declare external variables and functions from vterm package
(defvar vterm-buffer-name)
(defvar vterm-copy-mode)
(defvar vterm-environment)
(defvar vterm-shell)
(defvar vterm-term-environment-variable)
(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm--window-adjust-process-window-size "vterm" (process window))
(declare-function vterm-copy-mode "vterm" (&optional arg))
(declare-function vterm-mode "vterm")
(declare-function vterm-send-key "vterm" key &optional shift meta ctrl accept-proc-output)
(declare-function vterm-send-string "vterm" (string &optional paste-p))

;; Helper to ensure vterm is loaded
(cl-defmethod qwen-cli--term-make ((_backend (eql vterm)) buffer-name program &optional switches)
  "Create a vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm).
BUFFER-NAME is the name for the new terminal buffer.
PROGRAM is the program to run in the terminal.
SWITCHES are optional command-line arguments for PROGRAM."
  (qwen-cli--ensure-vterm)
  (let* ((vterm-shell (if switches
                          (concat program " " (mapconcat #'identity switches " "))
                        program))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; vterm needs to have an open window before starting the qwen
      ;; process; otherwise Qwen doesn't seem to know how wide its
      ;; terminal window is and it draws the input box too wide. But
      ;; the user may not want to pop to the buffer. For some reason
      ;; `display-buffer' also leads to wonky results, it has to be
      ;; `pop-to-buffer'. So, show the buffer, start vterm-mode (which
      ;; starts the vterm-shell qwen process), and then hide the
      ;; buffer. We'll optionally re-open it later.
      ;;
      ;; [TODO] see if there's a cleaner way to do this.
      (pop-to-buffer buffer)
      (vterm-mode)
      (delete-window (get-buffer-window buffer))
      buffer)))

(defun qwen-cli--ensure-vterm ()
  "Ensure vterm package is loaded."
  (unless (featurep 'vterm)
    (unless (require 'vterm nil t)
      (error "The vterm package is required for vterm terminal backend. Please install it"))))

(cl-defmethod qwen-cli--term-send-string ((_backend (eql vterm)) string)
  "Send STRING to vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm).
_TERMINAL is unused for vterm backend.
STRING is the text to send to the terminal."
  (vterm-send-string string))

(cl-defmethod qwen-cli--term-kill-process ((_backend (eql vterm)) buffer)
  "Kill the vterm terminal process in BUFFER.

_BACKEND is the terminal backend type (should be \\='vterm).
BUFFER is the terminal buffer containing the process to kill."
  (kill-process (get-buffer-process buffer)))

;; Mode operations
(cl-defmethod qwen-cli--term-read-only-mode ((_backend (eql vterm)))
  "Switch vterm terminal to read-only mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (qwen-cli--ensure-vterm)
  (vterm-copy-mode 1)
  (setq-local cursor-type t))

(cl-defmethod qwen-cli--term-interactive-mode ((_backend (eql vterm)))
  "Switch vterm terminal to interactive mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (qwen-cli--ensure-vterm)
  (vterm-copy-mode -1)
  (setq-local cursor-type nil))

(cl-defmethod qwen-cli--term-in-read-only-p ((_backend (eql vterm)))
  "Check if vterm terminal is in read-only mode.

_BACKEND is the terminal backend type (should be \\='vterm)."
  vterm-copy-mode)

(cl-defmethod qwen-cli--term-configure ((_backend (eql vterm)))
  "Configure vterm terminal in current buffer.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (qwen-cli--ensure-vterm)
  ;; set TERM
  (setq vterm-term-environment-variable qwen-cli-term-name)
  ;; Prevent vterm from automatically renaming the buffer
  (setq-local vterm-buffer-name-string nil)
  ;; Disable automatic scrolling to bottom on output to prevent flickering
  (setq-local vterm-scroll-to-bottom-on-output nil)
  ;; Disable immediate redraw to batch updates and reduce flickering
  (setq-local vterm--redraw-immididately nil)
  ;; Try to prevent cursor flickering by disabling Emacs' own cursor management
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local blink-cursor-mode nil)
  (setq-local cursor-type nil)  ; Let vterm handle the cursor entirely
  ;; Set timer delay to nil for faster updates (reduces visible flicker duration)
  ;; (setq-local vterm-timer-delay nil)
  ;; Increase process read buffering to batch more updates together
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)
    ;; Try to make vterm read larger chunks at once
    (process-put proc 'read-output-max 4096))
  ;; Set up bell detection advice
  (advice-add 'vterm--filter :around #'qwen-cli--vterm-bell-detector)
  ;; Set up multi-line buffering to prevent flickering
  (advice-add 'vterm--filter :around #'qwen-cli--vterm-multiline-buffer-filter))

(cl-defmethod qwen-cli--term-customize-faces ((_backend (eql vterm)))
  "Apply face customizations for vterm terminal.

_BACKEND is the terminal backend type (should be \\='vterm)."
  ;; no faces to customize yet (this could change)
  )

(defun qwen-cli--vterm-send-escape ()
  "Send escape key to vterm."
  (interactive)
  (vterm-send-key ""))

(defun qwen-cli--vterm-send-return ()
  "Send escape key to vterm."
  (interactive)
  (vterm-send-key ""))

(defun qwen-cli--vterm-send-alt-return ()
  "Send <alt>-<return> to vterm."
  (interactive)
  (vterm-send-key "" nil t))

(defun qwen-cli--vterm-send-shift-return ()
  "Send shift return to vterm."
  (interactive)
  (vterm-send-key "" t))

(defun qwen-cli--vterm-send-super-return ()
  "Send escape key to vterm."
  (interactive)
  ;; (vterm-send-key " " t)
  (vterm-send-key (kbd "s-<return>") t))

;; (defun qwen-cli--vterm-send-alt-return ()
;;   "Send alt-return to vterm for newline without submitting."
;;   (message "qwen-cli--vterm-send-alt-return invoked")
;;   (interactive)
;;   (vterm-send-key "" nil t))

(cl-defmethod qwen-cli--term-setup-keymap ((_backend (eql vterm)))
  "Set up the local keymap for Qwen CLI buffers.

_BACKEND is the terminal backend type (should be \\='vterm)."
  (let ((map (make-sparse-keymap)))
    ;; Inherit parent eat keymap
    (set-keymap-parent map (current-local-map))

    ;; C-g for escape
    (define-key map (kbd "C-g") #'qwen-cli--vterm-send-escape)

    (pcase qwen-cli-newline-keybinding-style
      ('newline-on-shift-return
       ;; S-return enters a line break, RET sends the command
       (define-key map (kbd "<S-return>") #'qwen-cli--vterm-send-alt-return)
       (define-key map (kbd "<return>") #'qwen-cli--vterm-send-return))
      ('newline-on-alt-return
       ;; M-return enters a line break, RET sends the command
       (define-key map (kbd "<M-return>") #'qwen-cli--vterm-send-alt-return)
       (define-key map (kbd "<return>") #'qwen-cli--vterm-send-return))
      ('shift-return-to-send
       ;; RET enters a line break, S-return sends the command
       (define-key map (kbd "<return>") #'qwen-cli--vterm-send-alt-return)
       (define-key map (kbd "<S-return>") #'qwen-cli--vterm-send-return))
      ('super-return-to-send
       ;; RET enters a line break, s-return sends the command.
       (define-key map (kbd "<return>") #'qwen-cli--vterm-send-alt-return)
       (define-key map (kbd "<s-return>") #'qwen-cli--vterm-send-return)))

    (use-local-map map)))

(cl-defmethod qwen-cli--term-get-adjust-process-window-size-fn ((_backend (eql vterm)))
  "Get the BACKEND specific function that adjusts window size."
  #'vterm--window-adjust-process-window-size)

;;;; Private util functions
(defmacro qwen-cli--with-buffer (&rest body)
  "Execute BODY with the Qwen buffer, handling buffer selection and display.

Gets or prompts for the Qwen buffer, executes BODY within that buffer's
context, displays the buffer, and shows not-running message if no buffer
is found."
  `(if-let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
       (with-current-buffer qwen-cli-buffer
         ,@body
         (display-buffer qwen-cli-buffer))
     (qwen-cli--show-not-running-message)))

(defun qwen-cli--buffer-p (buffer)
  "Return non-nil if BUFFER is a Qwen buffer.

BUFFER can be either a buffer object or a buffer name string."
  (let ((name (if (stringp buffer)
                  buffer
                (buffer-name buffer))))
    (and name (string-match-p "^\\*qwen:" name))))

(defun qwen-cli--directory ()
  "Get get the root Qwen directory for the current buffer.

If not in a project and no buffer file return `default-directory'."
  (let* ((project (project-current))
         (current-file (buffer-file-name)))
    (cond
     ;; Case 1: In a project
     (project (project-root project))
     ;; Case 2: Has buffer file (when not in VC repo)
     (current-file (file-name-directory current-file))
     ;; Case 3: No project and no buffer file
     (t default-directory))))

(defun qwen-cli--find-all-qwen-buffers ()
  "Find all active Qwen buffers across all directories.

Returns a list of buffer objects."
  (cl-remove-if-not
   #'qwen-cli--buffer-p
   (buffer-list)))

(defun qwen-cli--find-qwen-buffers-for-directory (directory)
  "Find all active Qwen buffers for a specific DIRECTORY.

Returns a list of buffer objects."
  (cl-remove-if-not
   (lambda (buf)
     (let ((buf-dir (qwen-cli--extract-directory-from-buffer-name (buffer-name buf))))
       (and buf-dir
            (string= (file-truename (abbreviate-file-name directory))
                     (file-truename buf-dir)))))
   (qwen-cli--find-all-qwen-buffers)))

(defun qwen-cli--extract-directory-from-buffer-name (buffer-name)
  "Extract the directory path from a Qwen BUFFER-NAME.

For example, *qwen:/path/to/project/* returns /path/to/project/.
For example, *qwen:/path/to/project/:tests* returns /path/to/project/."
  (when (string-match "^\\*qwen:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (match-string 1 buffer-name)))

(defun qwen-cli--extract-instance-name-from-buffer-name (buffer-name)
  "Extract the instance name from a Qwen BUFFER-NAME.

For example, *qwen:/path/to/project/:tests* returns \"tests\".
For example, *qwen:/path/to/project/* returns nil."
  (when (string-match "^\\*qwen:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name)
    (match-string 2 buffer-name)))

(defun qwen-cli--buffer-display-name (buffer)
  "Create a display name for Qwen BUFFER.

Returns a formatted string like `project:instance (directory)' or
`project (directory)'."
  (let* ((name (buffer-name buffer))
         (dir (qwen-cli--extract-directory-from-buffer-name name))
         (instance-name (qwen-cli--extract-instance-name-from-buffer-name name)))
    (if instance-name
        (format "%s:%s (%s)"
                (file-name-nondirectory (directory-file-name dir))
                instance-name
                dir)
      (format "%s (%s)"
              (file-name-nondirectory (directory-file-name dir))
              dir))))

(defun qwen-cli--buffers-to-choices (buffers &optional simple-format)
  "Convert BUFFERS list to an alist of (display-name . buffer) pairs.

If SIMPLE-FORMAT is non-nil, use just the instance name as display name."
  (mapcar (lambda (buf)
            (let ((display-name (if simple-format
                                    (or (qwen-cli--extract-instance-name-from-buffer-name
                                         (buffer-name buf))
                                        "default")
                                  (qwen-cli--buffer-display-name buf))))
              (cons display-name buf)))
          buffers))

(defun qwen-cli--select-buffer-from-choices (prompt buffers &optional simple-format)
  "Prompt user to select a buffer from BUFFERS list using PROMPT.

If SIMPLE-FORMAT is non-nil, use simplified display names.
Returns the selected buffer or nil."
  (when buffers
    (let* ((choices (qwen-cli--buffers-to-choices buffers simple-format))
           (selection (completing-read prompt
                                       (mapcar #'car choices)
                                       nil t)))
      (cdr (assoc selection choices)))))

(defun qwen-cli--prompt-for-qwen-buffer ()
  "Prompt user to select from available Qwen buffers.

Returns the selected buffer or nil if canceled. If a buffer is selected,
it's remembered for the current directory."
  (let* ((current-dir (qwen-cli--directory))
         (qwen-buffers (qwen-cli--find-all-qwen-buffers)))
    (when qwen-buffers
      (let* ((prompt (substitute-command-keys
                      (format "No Qwen instance running in %s. Cancel (\\[keyboard-quit]), or select Qwen instance: "
                              (abbreviate-file-name current-dir))))
             (selected-buffer (qwen-cli--select-buffer-from-choices prompt qwen-buffers)))
        ;; Remember the selection for this directory
        (when selected-buffer
          (puthash current-dir selected-buffer qwen-cli--directory-buffer-map))
        selected-buffer))))

(defun qwen-cli--get-or-prompt-for-buffer ()
  "Get Qwen buffer for current directory or prompt for selection.

First checks for Qwen buffers in the current directory. If there are
multiple, prompts the user to select one. If there are none, checks if
there's a remembered selection for this directory. If not, and there are
other Qwen buffers running, prompts the user to select one. Returns
the buffer or nil."
  (let* ((current-dir (qwen-cli--directory))
         (dir-buffers (qwen-cli--find-qwen-buffers-for-directory current-dir)))
    (cond
     ;; Multiple buffers for this directory - prompt for selection
     ((> (length dir-buffers) 1)
      (qwen-cli--select-buffer-from-choices
       (format "Select Qwen instance for %s: "
               (abbreviate-file-name current-dir))
       dir-buffers
       t))  ; Use simple format (just instance names)
     ;; Single buffer for this directory - use it
     ((= (length dir-buffers) 1)
      (car dir-buffers))
     ;; No buffers for this directory - check remembered or prompt for other directories
     (t
      ;; Check for remembered selection for this directory
      (let ((remembered-buffer (gethash current-dir qwen-cli--directory-buffer-map)))
        (if (and remembered-buffer (buffer-live-p remembered-buffer))
            remembered-buffer
          ;; No valid remembered buffer, check for other Qwen instances
          (let ((other-buffers (qwen-cli--find-all-qwen-buffers)))
            (when other-buffers
              (qwen-cli--prompt-for-qwen-buffer)))))))))

(defun qwen-cli--switch-to-selected-buffer (selected-buffer)
  "Switch to SELECTED-BUFFER if it's not the current buffer.

This is used after command functions to ensure we switch to the
selected Qwen buffer when the user chose a different instance."
  (when (and selected-buffer
             (not (eq selected-buffer (current-buffer))))
    (pop-to-buffer selected-buffer)))

(defun qwen-cli--buffer-name (&optional instance-name)
  "Generate the Qwen buffer name based on project or current buffer file.

If INSTANCE-NAME is provided, include it in the buffer name.
If not in a project and no buffer file, raise an error."
  (let ((dir (qwen-cli--directory)))
    (if dir
        (if instance-name
            (format "*qwen:%s:%s*" (abbreviate-file-name (file-truename dir)) instance-name)
          (format "*qwen:%s*" (abbreviate-file-name (file-truename dir))))
      (error "Cannot determine Qwen directory - no `default-directory'!"))))

(defun qwen-cli--prompt-for-instance-name (dir existing-instance-names &optional force-prompt)
  "Prompt user for a new instance name for directory DIR.

EXISTING-INSTANCE-NAMES is a list of existing instance names.
If FORCE-PROMPT is non-nil, always prompt even if no instances exist."
  (if (or existing-instance-names force-prompt)
      (let ((proposed-name ""))
        (while (or (string-empty-p proposed-name)
                   (member proposed-name existing-instance-names))
          (setq proposed-name
                (read-string (if (and existing-instance-names (not force-prompt))
                                 (format "Instances already running for %s (existing: %s), new instance name: "
                                         (abbreviate-file-name dir)
                                         (mapconcat #'identity existing-instance-names ", "))
                               (format "Instance name for %s: " (abbreviate-file-name dir)))
                             nil nil proposed-name))
          (cond
           ((string-empty-p proposed-name)
            (message "Instance name cannot be empty. Please enter a name.")
            (sit-for 1))
           ((member proposed-name existing-instance-names)
            (message "Instance name '%s' already exists. Please choose a different name." proposed-name)
            (sit-for 1))))
        proposed-name)
    "default"))

(defun qwen-cli--show-not-running-message ()
  "Show a message that Qwen is not running in any directory."
  (message "Qwen is not running"))

(defun qwen-cli--kill-buffer (buffer)
  "Kill a Qwen BUFFER by cleaning up hooks and processes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Remove the adjust window size advice if it was added
      (when qwen-cli-optimize-window-resize
        (advice-remove (qwen-cli--term-get-adjust-process-window-size-fn qwen-cli-terminal-backend) #'qwen-cli--adjust-window-size-advice))
      ;; Remove vterm advice if using vterm backend
      (when (eq qwen-cli-terminal-backend 'vterm)
        (advice-remove 'vterm--filter #'qwen-cli--vterm-bell-detector)
        (advice-remove 'vterm--filter #'qwen-cli--vterm-multiline-buffer-filter))
      ;; Clean the window widths hash table
      (when qwen-cli--window-widths
        (clrhash qwen-cli--window-widths))
      ;; Kill the process
      (qwen-cli--term-kill-process qwen-cli-terminal-backend buffer))))

(defun qwen-cli--cleanup-directory-mapping ()
  "Remove entries from directory-buffer map when this buffer is killed.

This function is added to `kill-buffer-hook' in Qwen buffers to clean up
the remembered directory->buffer associations."
  (let ((dying-buffer (current-buffer)))
    (maphash (lambda (dir buffer)
               (when (eq buffer dying-buffer)
                 (remhash dir qwen-cli--directory-buffer-map)))
             qwen-cli--directory-buffer-map)))

(defun qwen-cli--get-buffer-file-name ()
  "Get the file name associated with the current buffer."
  (when buffer-file-name
    (file-local-name (file-truename buffer-file-name))))

(defun qwen-cli--format-file-reference (&optional file-name line-start line-end)
  "Format a file reference in the @file:line style.

FILE-NAME is the file path.  If nil, get from current buffer.
LINE-START is the starting line number.  If nil, use current line.
LINE-END is the ending line number for a range.  If nil, format single line."
  (let ((file (or file-name (qwen-cli--get-buffer-file-name)))
        (start (or line-start (line-number-at-pos)))
        (end line-end))
    (when file
      (if end
          (format "@%s:%d-%d" file start end)
        (format "@%s:%d" file start)))))

(defun qwen-cli--do-send-command (cmd)
  "Send a command CMD to Qwen if Qwen buffer exists.

After sending the command, move point to the end of the buffer.
Returns the selected Qwen buffer or nil."
  (if-let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
      (progn
        (with-current-buffer qwen-cli-buffer
          (qwen-cli--term-send-string qwen-cli-terminal-backend cmd)
          (sleep-for 0.1)
          (qwen-cli--term-send-string qwen-cli-terminal-backend (kbd "RET"))
          (display-buffer qwen-cli-buffer))
        qwen-cli-buffer)
    (qwen-cli--show-not-running-message)
    nil))

(defun qwen-cli--start (arg extra-switches &optional force-prompt force-switch-to-buffer)
  "Start Qwen with given command-line EXTRA-SWITCHES.

ARG is the prefix argument controlling directory and buffer switching.
EXTRA-SWITCHES is a list of additional command-line switches to pass to Qwen.
If FORCE-PROMPT is non-nil, always prompt for instance name.
If FORCE-SWITCH-TO-BUFFER is non-nil, always switch to the Qwen buffer.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (let* ((dir (if (equal arg '(16))     ; Double prefix
                  (read-directory-name "Project directory: ")
                (qwen-cli--directory)))
         (switch-after (or (equal arg '(4)) force-switch-to-buffer)) ; Single prefix or force-switch-to-buffer
         (default-directory dir)
         ;; Check for existing Qwen instances in this directory
         (existing-buffers (qwen-cli--find-qwen-buffers-for-directory dir))
         ;; Get existing instance names
         (existing-instance-names (mapcar (lambda (buf)
                                            (or (qwen-cli--extract-instance-name-from-buffer-name
                                                 (buffer-name buf))
                                                "default"))
                                          existing-buffers))
         ;; Prompt for instance name (only if instances exist, or force-prompt is true)
         (instance-name (qwen-cli--prompt-for-instance-name dir existing-instance-names force-prompt))
         (buffer-name (qwen-cli--buffer-name instance-name))
         (program-switches (if extra-switches
                               (append qwen-cli-program-switches extra-switches)
                             qwen-cli-program-switches))

         ;; Set process-adaptive-read-buffering to nil to avoid flickering while Qwen is processing
         (process-adaptive-read-buffering nil)

         ;; Start the terminal process
         (buffer (qwen-cli--term-make qwen-cli-terminal-backend buffer-name qwen-cli-program program-switches)))

    ;; Check if the qwen program is available
    (unless (executable-find qwen-cli-program)
      (error "Qwen CLI program '%s' not found in PATH" qwen-cli-program))

    ;; Check if buffer was successfully created
    (unless (buffer-live-p buffer)
      (error "Failed to create Qwen CLI buffer"))

    ;; setup qwen buffer
    (with-current-buffer buffer

      ;; Configure terminal with backend-specific settings
      (qwen-cli--term-configure qwen-cli-terminal-backend)

      ;; Initialize the window widths hash table
      (setq qwen-cli--window-widths (make-hash-table :test 'eq :weakness 'key))

      ;; Set up window width tracking if optimization is enabled
      (when qwen-cli-optimize-window-resize
        (advice-add (qwen-cli--term-get-adjust-process-window-size-fn qwen-cli-terminal-backend) :around #'qwen-cli--adjust-window-size-advice))

      ;; Setup our custom key bindings
      (qwen-cli--term-setup-keymap qwen-cli-terminal-backend)

      ;; Customize terminal faces
      (qwen-cli--term-customize-faces qwen-cli-terminal-backend)

      ;; remove underlines from _>_
      (face-remap-add-relative 'nobreak-space :underline nil)

      ;; set buffer face
      (buffer-face-set :inherit 'qwen-cli-repl-face)

      ;; disable scroll bar, fringes
      (setq-local vertical-scroll-bar nil)
      (setq-local fringe-mode 0)

      ;; Add cleanup hook to remove directory mappings when buffer is killed
      (add-hook 'kill-buffer-hook #'qwen-cli--cleanup-directory-mapping nil t)

      ;; run start hooks
      (run-hooks 'qwen-cli-start-hook)

      ;; Disable vertical scroll bar in qwen buffer
      (setq-local vertical-scroll-bar nil)

      ;; Display buffer, setting window parameters
      (let ((window (display-buffer-in-side-window buffer '((side . right)(window-width . 0.4)))))
        (when window
          ;; turn off fringes and margins in the Qwen buffer
          (set-window-parameter window 'left-margin-width 0)
          (set-window-parameter window 'right-margin-width 0)
          (set-window-parameter window 'left-fringe-width 0)
          (set-window-parameter window 'right-fringe-width 0)
          ;; set no-delete-other-windows parameter for qwen-cli window
          (set-window-parameter window 'no-delete-other-windows qwen-cli-no-delete-other-windows))))

    ;; switch to the Qwen buffer if asked to
    (when switch-after
      (pop-to-buffer buffer))))

;;;###autoload
(defun qwen-cli (&optional arg)
  "Start Qwen in an eat terminal and enable `qwen-cli-mode'.

If current buffer belongs to a project start Qwen in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")
  (qwen-cli--start arg nil))

;;;###autoload
(defun qwen-cli-start-in-directory (&optional arg)
  "Prompt for a directory and start Qwen there.

This is a convenience command equivalent to using `qwen-cli` with
double prefix arg (\\[universal-argument] \\[universal-argument]).

With prefix ARG (\\[universal-argument]), switch to buffer after creating."
  (interactive "P")
  ;; Always prompt for directory (like double prefix)
  ;; If user gave us a prefix arg, also switch to buffer after creating
  (let ((dir (read-directory-name "Project directory: ")))
    ;; We need to temporarily override qwen-cli--directory to return our chosen dir
    (cl-letf (((symbol-function 'qwen-cli--directory) (lambda () dir)))
      (qwen-cli (when arg '(4))))))

;;;###autoload
(defun qwen-cli-continue (&optional arg)
  "Start Qwen and continue the previous conversation.

This command starts Qwen with the --continue flag to resume
where you left off in your last session.

If current buffer belongs to a project start Qwen in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")
  (qwen-cli--start arg '("--continue")))

;;;###autoload
(defun qwen-cli-resume (arg)
  "Resume a specific Qwen session.

This command starts Qwen with the --resume flag to resume a specific
past session. Qwen will present an interactive list of past sessions
to choose from.

If current buffer belongs to a project start Qwen in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
 buffer file.

With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt for the project directory."
  (interactive "P")

  (let ((extra-switches '("--resume")))
    (qwen-cli--start arg extra-switches nil t))
  (qwen-cli--term-send-string qwen-cli-terminal-backend "")
  (goto-char (point-min)))

;;;###autoload
(defun qwen-cli-new-instance (&optional arg)
  "Create a new Qwen instance, prompting for instance name.

This command always prompts for an instance name, unlike `qwen-cli'
which uses \"default\" when no instances exist.

If current buffer belongs to a project start Qwen in the project's
root directory. Otherwise start in the directory of the current buffer
file, or the current value of `default-directory' if no project and no
buffer file.

With single prefix ARG (\\[universal-argument]), switch to buffer after creating.
With double prefix ARG (\\[universal-argument] \\[universal-argument]), prompt
for the project directory."
  (interactive "P")

  ;; Call qwen-cli--start with force-prompt=t
  (qwen-cli--start arg nil t))

(defun qwen-cli--format-errors-at-point ()
  "Format errors at point as a string with file and line numbers.
First tries flycheck errors if flycheck is enabled, then falls back
to help-at-pt (used by flymake and other systems).
Returns a string with the errors or a message if no errors found."
  (interactive)
  (cond
   ;; Try flycheck first if available and enabled
   ((and (featurep 'flycheck) (bound-and-true-p flycheck-mode))
    (let ((errors (flycheck-overlay-errors-at (point)))
          (result ""))
      (if (not errors)
          "No flycheck errors at point"
        (dolist (err errors)
          (let ((file (flycheck-error-filename err))
                (line (flycheck-error-line err))
                (msg (flycheck-error-message err)))
            (setq result (concat result
                                 (format "%s:%d: %s\n"
                                         file
                                         line
                                         msg)))))
        (string-trim-right result))))
   ;; Fall back to help-at-pt-kbd-string (works with flymake and other sources)
   ((help-at-pt-kbd-string)
    (let ((help-str (help-at-pt-kbd-string)))
      (if (not (null help-str))
          (substring-no-properties help-str)
        "No help string available at point")))
   ;; No errors found by any method
   (t "No errors at point")))

(defun qwen-cli--pulse-modeline ()
  "Pulse the modeline to provide visual notification."
  ;; First pulse - invert
  (invert-face 'mode-line)
  (run-at-time 0.1 nil
               (lambda ()
                 ;; Return to normal
                 (invert-face 'mode-line)
                 ;; Second pulse
                 (run-at-time 0.1 nil
                              (lambda ()
                                (invert-face 'mode-line)
                                ;; Final return to normal
                                (run-at-time 0.1 nil
                                             (lambda ()
                                               (invert-face 'mode-line))))))))

(defun qwen-cli-default-notification (title message)
  "Default notification function that displays a message and pulses the modeline.

TITLE is the notification title.
MESSAGE is the notification body."
  ;; Display the message
  (message "%s: %s" title message)
  ;; Pulse the modeline for visual feedback
  (qwen-cli--pulse-modeline)
  (message "%s: %s" title message))

(defun qwen-cli--notify (_terminal)
  "Notify the user that Qwen has finished and is awaiting input.

TERMINAL is the eat terminal parameter (not used)."
  (when qwen-cli-enable-notifications
    (funcall qwen-cli-notification-function
             "Qwen Ready"
             "Waiting for your response")))

(defun qwen-cli--vterm-bell-detector (orig-fun process input)
  "Detect bell characters in vterm output and trigger notifications.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (when (and (string-match-p "\007" input)
             (buffer-local-value 'qwen-cli-mode (process-buffer process))
             ;; Ignore bells in OSC sequences (terminal title updates)
             (not (string-match-p "]0;.*\007" input)))
    (qwen-cli--notify nil))

  (funcall orig-fun process input))

(defvar-local qwen-cli--vterm-multiline-buffer nil
  "Buffer for accumulating multi-line vterm output.")

(defvar-local qwen-cli--vterm-multiline-buffer-timer nil
  "Timer for processing buffered multi-line vterm output.")

(defun qwen-cli--vterm-multiline-buffer-filter (orig-fun process input)
  "Buffer vterm output when it appears to be redrawing multi-line input.
This prevents flickering when Qwen redraws its input box as it expands
to multiple lines. We detect this by looking for escape sequences that
indicate cursor positioning and line clearing operations.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (if (not qwen-cli-vterm-buffer-multiline-output)
      ;; Feature disabled, pass through normally
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Check if this looks like multi-line input box redraw
      ;; Common patterns when redrawing multi-line input:
      ;; - ESC[K (clear to end of line)
      ;; - ESC[<n>;<m>H (cursor positioning)
      ;; - ESC[<n>A/B/C/D (cursor movement)
      ;; - Multiple of these in sequence
      (let ((has-clear-line (string-match-p "\033\\[K" input))
            (has-cursor-pos (string-match-p "\033\\[[0-9]+;[0-9]+H" input))
            (has-cursor-move (string-match-p "\033\\[[0-9]*[ABCD]" input))
            (escape-count (cl-count ?\033 input)))

        ;; If we see multiple escape sequences that look like redrawing,
        ;; or we're already buffering, add to buffer
        (if (or (and (>= escape-count 3)
                     (or has-clear-line has-cursor-pos has-cursor-move))
                qwen-cli--vterm-multiline-buffer)
            (progn
              ;; Add to buffer
              (setq qwen-cli--vterm-multiline-buffer
                    (concat qwen-cli--vterm-multiline-buffer input))
              ;; Cancel existing timer
              (when qwen-cli--vterm-multiline-buffer-timer
                (cancel-timer qwen-cli--vterm-multiline-buffer-timer))
              ;; Set timer with configurable delay
              ;; This is enough to collect a burst of updates but not noticeable to user
              (setq qwen-cli--vterm-multiline-buffer-timer
                    (run-at-time qwen-cli-vterm-multiline-delay nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when qwen-cli--vterm-multiline-buffer
                                         (let ((inhibit-redisplay t)
                                               (data qwen-cli--vterm-multiline-buffer))
                                           ;; Clear buffer first to prevent recursion
                                           (setq qwen-cli--vterm-multiline-buffer nil
                                                 qwen-cli--vterm-multiline-buffer-timer nil)
                                           ;; Process all buffered data at once
                                           (funcall orig-fun
                                                    (get-buffer-process buf)
                                                    data))))))
                                 (current-buffer))))
          ;; Not multi-line redraw, process normally
          (funcall orig-fun process input))))))

(defun qwen-cli--adjust-window-size-advice (orig-fun &rest args)
  "Advice to only signal on width change.

Works with `eat--adjust-process-window-size' or
`vterm--adjust-process-window-size' to prevent unnecessary reflows.

Returns the size returned by ORIG-FUN only when the width of any Qwen
window has changed, not when only the height has changed. This prevents
unnecessary terminal reflows when only vertical space changes.

ARGS is passed to ORIG-FUN unchanged."
  (let ((result (apply orig-fun args)))
    ;; Check all windows for Qwen buffers
    (let ((width-changed nil))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (and buffer (qwen-cli--buffer-p buffer))
            (let ((current-width (window-width window))
                  (stored-width (gethash window qwen-cli--window-widths)))
              ;; Check if this is a new window or if width changed
              (when (or (not stored-width) (/= current-width stored-width))
                (setq width-changed t)
                ;; Update stored width
                (puthash window current-width qwen-cli--window-widths))))))
      ;; Return result only if a Qwen window width changed and
      ;; we're not in read-only mode. otherwise nil. Nil means do
      ;; not send a window size changed event to the Qwen process.
      (if (and width-changed (not (qwen-cli--term-in-read-only-p qwen-cli-terminal-backend)))
          result
        nil))))

;;;; Interactive Commands

;;;###autoload
(defun qwen-cli-send-region (&optional arg)
  "Send the current region to Qwen.

If no region is active, send the entire buffer if it's not too large.
For large buffers, ask for confirmation first.

With prefix ARG, prompt for instructions to add to the text before
sending. With two prefix ARGs (C-u C-u), both add instructions and
switch to Qwen buffer."
  (interactive "P")
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (if (> (buffer-size) qwen-cli-large-buffer-threshold)
                     (when (yes-or-no-p "Buffer is large.  Send anyway? ")
                       (buffer-substring-no-properties (point-min) (point-max)))
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (prompt (cond
                  ((equal arg '(4))     ; C-u
                   (read-string "Instructions: "))
                  (t nil)))
         (full-text (if prompt
                        (format "%s\n\n%s" prompt text)
                      text)))
    (when full-text
      (let ((selected-buffer (qwen-cli--do-send-command full-text)))
        (when (and (equal arg '(16)) selected-buffer) ; Only switch buffer with C-u C-u
          (pop-to-buffer selected-buffer))))))

;;;###autoload
(defun qwen-cli-toggle ()
  "Show or hide the Qwen window.

If the Qwen buffer doesn't exist, create it."
  (interactive)
  (let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
    (if qwen-cli-buffer
        (if (get-buffer-window qwen-cli-buffer)
            (delete-window (get-buffer-window qwen-cli-buffer))
      (let ((window (display-buffer-in-side-window qwen-cli-buffer '((side . right)(window-width . 0.4)))))
            ;; set no-delete-other-windows parameter for qwen-cli window
            (set-window-parameter window 'no-delete-other-windows qwen-cli-no-delete-other-windows)))
      (qwen-cli--show-not-running-message))))

;;;###autoload
(defun qwen-cli--switch-to-all-instances-helper ()
  "Helper function to switch to a Qwen buffer from all available instances.

Returns t if a buffer was selected and switched to, nil otherwise."
  (let ((all-buffers (qwen-cli--find-all-qwen-buffers)))
    (cond
     ((null all-buffers)
      (qwen-cli--show-not-running-message)
      nil)
     ((= (length all-buffers) 1)
      ;; Only one buffer, just switch to it
      (pop-to-buffer (car all-buffers))
      t)
     (t
      ;; Multiple buffers, let user choose
      (let ((selected-buffer (qwen-cli--select-buffer-from-choices
                              "Select Qwen instance: "
                              all-buffers)))
        (when selected-buffer
          (pop-to-buffer selected-buffer)
          t))))))

(defun qwen-cli-switch-to-buffer (&optional arg)
  "Switch to the Qwen buffer if it exists.

With prefix ARG, show all Qwen instances across all directories."
  (interactive "P")
  (if arg
      ;; With prefix arg, show all Qwen instances
      (qwen-cli--switch-to-all-instances-helper)
    ;; Without prefix arg, use normal behavior
    (if-let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
        (pop-to-buffer qwen-cli-buffer)
      (qwen-cli--show-not-running-message))))

;;;###autoload
(defun qwen-cli-select-buffer ()
  "Select and switch to a Qwen buffer from all running instances.

This command shows all Qwen instances across all projects and
directories, allowing you to choose which one to switch to."
  (interactive)
  (qwen-cli--switch-to-all-instances-helper))

(defun qwen-cli--kill-all-instances ()
  "Kill all Qwen instances across all directories."
  (let ((all-buffers (qwen-cli--find-all-qwen-buffers)))
    (if all-buffers
        (let* ((buffer-count (length all-buffers))
               (plural-suffix (if (= buffer-count 1) "" "s")))
          (if qwen-cli-confirm-kill
              (when (yes-or-no-p (format "Kill %d Qwen instance%s? " buffer-count plural-suffix))
                (dolist (buffer all-buffers)
                  (qwen-cli--kill-buffer buffer))
                (message "%d Qwen instance%s killed" buffer-count plural-suffix))
            (dolist (buffer all-buffers)
              (qwen-cli--kill-buffer buffer))
            (message "%d Qwen instance%s killed" buffer-count plural-suffix)))
      (qwen-cli--show-not-running-message))))

;;;###autoload
(defun qwen-cli-kill ()
  "Kill Qwen process and close its window."
  (interactive)
  (if-let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
      (if qwen-cli-confirm-kill
          (when (yes-or-no-p "Kill Qwen instance? ")
            (qwen-cli--kill-buffer qwen-cli-buffer)
            (message "Qwen instance killed"))
        (qwen-cli--kill-buffer qwen-cli-buffer)
        (message "Qwen instance killed"))
    (qwen-cli--show-not-running-message)))

;;;###autoload
(defun qwen-cli-kill-all ()
  "Kill ALL Qwen processes across all directories."
  (interactive)
  (qwen-cli--kill-all-instances))

;;;###autoload
(defun qwen-cli-send-command (&optional arg)
  "Read a Qwen command from the minibuffer and send it.

With prefix ARG, switch to the Qwen buffer after sending CMD."
  (interactive)
  (let ((selected-buffer (qwen-cli--do-send-command
                          (read-string "Prompt: "))))
    (when (and arg selected-buffer)
      (pop-to-buffer selected-buffer))))

;;;##autoload
(defun qwen-cli-send-shell (cmd &optional arg)
  "Read a Qwen command from the minibuffer and send it.

With prefix ARG, switch to the Qwen buffer after sending CMD."
  (interactive "sQwen command: !\nP")
  (let ((selected-buffer (qwen-cli--do-send-command (concat "!" cmd))))
    (when selected-buffer
      (with-current-buffer selected-buffer
        (qwen-cli--do-send-command "!")))
    (when (and arg selected-buffer)
      (pop-to-buffer selected-buffer))))

;;;###autoload
(defun qwen-cli-send-command-with-context (&optional arg)
  "Read a Qwen command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Qwen buffer after sending CMD."
  (interactive)
  (let* ((cmd (read-string "Prompt:"))
         (file-ref (if (use-region-p)
                       (qwen-cli--format-file-reference
                        nil
                        (line-number-at-pos (region-beginning))
                        (line-number-at-pos (region-end)))
                     (qwen-cli--format-file-reference)))
         (cmd-with-context (if file-ref
                               (format "%s\n%s" cmd file-ref)
                             cmd)))
    (let ((selected-buffer (qwen-cli--do-send-command cmd-with-context)))
      (when (and arg selected-buffer)
        (pop-to-buffer selected-buffer)))))

;;;###autoload
(defun qwen-cli-send-return ()
  "Send <return> to the Qwen CLI REPL.

This is useful for saying Yes when Qwen asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (qwen-cli--do-send-command ""))

;;;###autoload
(defun qwen-cli-send-1 ()
  "Send \"1\" to the Qwen CLI REPL.

This selects the first option when Qwen presents a numbered menu."
  (interactive)
  (qwen-cli--do-send-command "1"))

;;;###autoload
(defun qwen-cli-send-2 ()
  "Send \"2\" to the Qwen CLI REPL.

This selects the second option when Qwen presents a numbered menu."
  (interactive)
  (qwen-cli--do-send-command "2"))

;;;###autoload
(defun qwen-cli-send-3 ()
  "Send \"3\" to the Qwen CLI REPL.

This selects the third option when Qwen presents a numbered menu."
  (interactive)
  (qwen-cli--do-send-command "3"))

;;;###autoload
(defun qwen-cli-send-4 ()
  "Send \"4\" to the Qwen CLI REPL.

This selects the third option when Qwen presents a numbered menu."
  (interactive)
  (qwen-cli--do-send-command "4"))

;;;###autoload
(defun qwen-cli-send-escape ()
  "Send <escape> to the Qwen CLI REPL.

This is useful for saying \"No\" when Qwen asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (qwen-cli--with-buffer
   (qwen-cli--term-send-string qwen-cli-terminal-backend (kbd "ESC"))))

;;;###autoload
(defun qwen-cli-send-file (file-path)
  "Send the specified FILE-PATH to Qwen prefixed with `@'.

FILE-PATH should be an absolute path to the file to send."
  (interactive "fFile to send to Qwen: ")
  (let ((command (format "@%s" (expand-file-name file-path))))
    (qwen-cli--do-send-command command)))

;;;###autoload
(defun qwen-cli-send-buffer-file (&optional arg)
  "Send the file associated with current buffer to Qwen prefixed with `@'.

With prefix ARG, prompt for instructions to add to the file before sending.
With two prefix ARGs, both add instructions and switch to Qwen buffer."
  (interactive "P")
  (let ((file-path (qwen-cli--get-buffer-file-name)))
    (if file-path
        (let* ((prompt (when arg
                        (read-string "Instructions: ")))
               (command (if prompt
                           (format "%s\n\n@%s" prompt file-path)
                         (format "@%s" file-path))))
          (let ((selected-buffer (qwen-cli--do-send-command command)))
            (when (and arg selected-buffer)
              (pop-to-buffer selected-buffer))))
      (error "Current buffer is not associated with a file"))))

(defun qwen-cli--send-meta-return ()
  "Send Meta-Return key sequence to the terminal."
  (interactive)
  (qwen-cli--term-send-string qwen-cli-terminal-backend "\e\C-m"))

(defun qwen-cli--send-return ()
  "Send Return key to the terminal."
  (interactive)
  (qwen-cli--term-send-string qwen-cli-terminal-backend (kbd "RET")))

;;;###autoload
(defun qwen-cli-cycle-mode ()
  "Send Shift-Tab to Qwen to cycle between modes.

Qwen uses Shift-Tab to cycle through:
- Default mode
- Auto-accept edits mode
- Plan mode"
  (interactive)
  (qwen-cli--with-buffer
   (qwen-cli--term-send-string qwen-cli-terminal-backend "\e[Z")))

;; (define-key key-translation-map (kbd "ESC") "")

;;;###autoload
(defun qwen-cli-fork ()
  "Jump to a previous conversation by invoking the Qwen fork command.

Sends <escape><escape> to the Qwen CLI REPL."
  (interactive)
  (if-let ((qwen-cli-buffer (qwen-cli--get-or-prompt-for-buffer)))
      (with-current-buffer qwen-cli-buffer
        (qwen-cli--term-send-string qwen-cli-terminal-backend "")
        ;; (display-buffer qwen-cli-buffer)
        (pop-to-buffer qwen-cli-buffer))
    (qwen-cli--show-not-running-message)))

;;;###autoload
(defun qwen-cli-fix-error-at-point (&optional arg)
  "Ask Qwen to fix the error at point.

Gets the error message, file name, and line number, and instructs Qwen
to fix the error. Supports both flycheck and flymake error systems, as well
as any system that implements help-at-pt.

With prefix ARG, switch to the Qwen buffer after sending."
  (interactive "P")
  (let* ((error-text (qwen-cli--format-errors-at-point))
         (file-ref (qwen-cli--format-file-reference)))
    (if (string= error-text "No errors at point")
        (message "No errors found at point")
      (let ((command (format "Fix this error at %s:\nDo not run any external linter or other program, just fix the error at point using the context provided in the error message: <%s>"
                             (or file-ref "current position") error-text)))
        (let ((selected-buffer (qwen-cli--do-send-command command)))
          (when (and arg selected-buffer)
            (pop-to-buffer selected-buffer)))))))

;;;###autoload
(defun qwen-cli-read-only-mode ()
  "Enter read-only mode in Qwen buffer with visible cursor.

In this mode, you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Qwen
buffer. However, you are not allowed to change the buffer contents or
enter Qwen commands.

Use `qwen-cli-exit-read-only-mode' to switch back to normal mode."
  (interactive)
  (qwen-cli--with-buffer
   (qwen-cli--term-read-only-mode qwen-cli-terminal-backend)
   (message "Qwen read-only mode enabled")))

;;;###autoload
(defun qwen-cli-exit-read-only-mode ()
  "Exit read-only mode and return to normal mode (eat semi-char mode)."
  (interactive)
  (qwen-cli--with-buffer
   (qwen-cli--term-interactive-mode qwen-cli-terminal-backend)
   (message "Qwen read-only disabled")))

;;;###autoload
(defun qwen-cli-toggle-read-only-mode ()
  "Toggle between read-only mode and normal mode.

In read-only mode you can interact with the terminal buffer just like a
regular buffer. This mode is useful for selecting text in the Qwen
buffer. However, you are not allowed to change the buffer contents or
enter Qwen commands."
  (interactive)
  (qwen-cli--with-buffer
   (if (not (qwen-cli--term-in-read-only-p qwen-cli-terminal-backend))
       (qwen-cli-read-only-mode)
     (qwen-cli-exit-read-only-mode))))

;;;; Mode definition
;;;###autoload
(define-minor-mode qwen-cli-mode
  "Minor mode for interacting with Qwen AI CLI.

When enabled, provides functionality for starting, sending commands to,
and managing Qwen sessions."
  :init-value nil
  :lighter " Qwen"
  :global t
  :group 'qwen-cli)

;;;; Provide the feature
(provide 'qwen-cli)

;;; qwen-cli.el ends here
