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

## Configuration Note
Your configuration is in `~/.emacs.d/init.el`.
- API Keys: Stored securely in `secrets.el` (ignored by git).
- Custom Settings: Auto-generated settings (like safe variables) are in `custom.el`.
- Packages: `neotree` (file tree), `ibuffer` (buffer list), `projectile` (project management), and `vertico` (modern completion).