# Emacs Basics
## Essential Commands

| Action | Key Sequence | Mnemonic |
| :--- | :--- | :--- |
| Quit Emacs | `C-x C-c` | Close / Exit |
| Open/Create File | `C-x C-f` | Find file |
| Save File | `C-x C-s` | Save |
| Cancel Command | `C-g` | Get me out! (Stop current action) |
| Close Current File | `C-x k` | Kill buffer |
| Switch Window | `C-x o` | Jump to Other window |
| Undo | `C-/` | Undo last action |
| Redo | `C-g C-/` | Redo action (after undo) |

## File Navigation (Vertico)

When using `C-x C-f` (Find file) or other file prompts with Vertico:

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Delete Char/Go Up | `DEL` (Backspace) | Smarter deletion; goes up directory level at path boundaries |
| Delete Word/Go Up | `M-DEL` (Option + Backspace) | Delete entire path component (e.g., folder name) |

## File Explorer (Neotree)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Toggle Sidebar | `C-x t t` | Open/Close the file tree |
| Find File in Tree | `C-x t f` | Reveal current file in sidebar |

## Buffer Management (Ibuffer)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| List Buffers | `C-x C-b` | Open a powerful list of all active buffers |

## Project Management (Projectile)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Find File in Project | `C-c p f` | Fuzzy search for files in current project |
| Switch Project | `C-c p p` | Switch to another project |
| Search in Project | `C-c p s g` | Grep (search text) in all project files |

## Org Mode

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Open Agenda | `C-c a` | View tasks, deadlines, and schedules |
| Capture Note/Task | `C-c c` | Quickly capture ideas into `~/dev/inbox.org` |
| Cycle TODO State | `C-c C-t` | Change task status (TODO, DOING, DONE, etc.) |

## Terminal (Vterm)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Toggle Terminal | `C-c t v` | Open/Close a fast terminal in a split window |

## Copying & Pasting

### General Copy/Paste
1.  Select Text:
    *   Move cursor to the start of the text.
    *   Press `C-SPC` (Control + Space) to "Set Mark".
    *   Move cursor to the end of the text to highlight it.
2.  Copy:
    *   Press `M-w` (Meta + w). *Note: On Mac, Meta is usually the Option key.*
3.  Paste (Yank):
    *   Press `C-y` to paste.

### Copying from the Messages Buffer
To copy system warnings or error messages:
1.  Switch to the Messages buffer:
    *   Press `C-x b`.
    *   Type `*Messages*` and press `Enter`.
2.  Navigate to the message you want to copy.
3.  Select and copy the text using the Select and Copy steps above.

## Movement Keys (Vim-like with Evil Mode)

By default, Emacs uses `Ctrl` and `Meta` key combinations for movement.

| Action                   | Key Sequence      |
| :--- | :--- |
| Move up line             | `C-p`             |
| Move down line           | `C-n`             |
| Move forward character   | `C-f`             |
| Move backward character  | `C-b`             |
| Move to start of line    | `C-a`             |
| Move to end of line      | `C-e`             |
| Move word forward        | `M-f`             |
| Move word backward       | `M-b`             |
| Move to next paragraph   | `M-}`             |
| Move to previous paragraph | `M-{`           |
| Scroll down (forward)    | `C-v`             |
| Scroll up (backward)     | `M-v`             |
| Go to beginning of buffer | `M-<`             |
| Go to end of buffer      | `M->`             |

For true Vim-like movement and modal editing (Normal, Insert, Visual modes), you will need to install the `evil-mode` package. `evil-mode` provides a nearly complete emulation of Vim within Emacs.

## Window Management

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Shrink Horizontally | `C-c <` | Make window narrower |
| Enlarge Horizontally | `C-c >` | Make window wider |
| Shrink Vertically | `C-c -` | Make window shorter |
| Enlarge Vertically | `C-c +` | Make window taller |
| Mouse Drag | `Left Click + Drag` | Drag status bar or divider to resize (if enabled) |

## Configuration Note
Your configuration is in `~/.emacs.d/init.el`.
- API Keys: Stored securely in `secrets.el` (ignored by git).
- Custom Settings: Auto-generated settings (like safe variables) are in `custom.el`.
- Packages: `neotree` (file tree), `ibuffer` (buffer list), `projectile` (project management), `vertico` (completion), `vterm` (terminal), and `markdown-mode`.
