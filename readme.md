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
| Toggle Read-Only | `C-x C-q` | Toggle buffer read-only state |

## File Navigation (Vertico)

When using `C-x C-f` (Find file) or other file prompts with Vertico:

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Delete Char/Go Up | `DEL` (Backspace) | Smarter deletion; goes up directory level at path boundaries |
| Delete Word/Go Up | `M-DEL` (Option + Backspace) | Delete entire path component (e.g., folder name) |

## File Explorer (Treemacs)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Toggle Sidebar | `C-x t t` | Open/Close the file tree |
| Find File in Tree | `C-x t C-t` | Reveal current file in sidebar |
| Select Window | `M-0` | Select the treemacs window |
| Bookmarks | `C-x t B` | Manage bookmarks in treemacs |
| Create File | `c f` | Create a new file at the current location or under the selected directory. |
| Create Directory | `c d` | Create a new directory at the current location or under the selected directory. |
| Move File/Directory | `m` | Move the selected file or directory to a new location. |
| Open Externally | `O` (Shift+o) | Open selected file in default system app (e.g., Preview) |
| Open in Ghostty | `T` (Shift+t) | Open Ghostty terminal in selected directory |
| Copy Path | `C` (Shift+c) | Copy the full path of the selected file or directory to the clipboard |
| Add Project | `C-c C-p` | Add a project to the workspace (when in Treemacs) |
| Remove Project | `C-c C-d` | Remove a project from the workspace (when in Treemacs) |

## Ebook Reading (CalibreDB & Nov.el)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Open CalibreDB | `C-c e` | Open the Calibre database browser |
| Open Book | `RET` (Enter) | In CalibreDB, open the selected EPUB book for reading in `nov-mode` |
| Search Books | `s` | In CalibreDB, search for books by keyword |
| List EPUB Files | `o` | In CalibreDB, list the internal files of the selected EPUB |
| Next Chapter | `n` | Go to the next chapter in `nov-mode` |
| Previous Chapter | `p` | Go to the previous chapter in `nov-mode` |

## Markdown Preview

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Toggle Live Preview | `C-c C-p` | Toggles live HTML preview for Markdown files (requires `markdown-mode-live-preview` or similar package) |

## Other Useful Commands

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Open Readme | `C-c r` | Custom shortcut to open this README file |
| Open Externally | `C-c o` | Open file in default system app (e.g., Preview) |
| Check Major Mode | `C-h v major-mode` | Check the current buffer's major mode |
| Describe Mode | `C-h m` | Display documentation for the current major and minor modes |

## Evil Mode (Vim Keybindings)

Evil Mode provides Vim-like keybindings in Emacs.

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Normal Mode | `ESC` or `C-[` | Default mode for navigation and commands |
| Insert Mode | `i`, `a`, `o`, `I`, `A`, `O` | For inserting text |
| Visual Mode | `v`, `V`, `C-v` | For selecting text |
| Emacs State | `C-z` | Toggle between Evil states and Emacs original behavior |

**Note**: `calibredb` buffers are configured to use Emacs' native keybindings by default, overriding Evil Mode for a more intuitive experience with book management.

## Journal Shortcuts

Quickly open your current journal files.

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Open Daily Journal | `C-c j d` | Opens `~/dev/journal/daily/YYYY/MM/YYYY-MM-DD.md` |
| Open Weekly Journal | `C-c j w` | Opens `~/dev/journal/weekly/YYYY/week-WW.md` |
| Open Monthly Journal | `C-c j m` | Opens `~/dev/journal/monthly/YYYY/MM-MonthName.md` |

## Buffer Management (Consult)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Switch Buffer | `C-x b` | Enhanced buffer switcher with previews and grouping |
| Search in File | `C-s` | Visual search within the current file (powered by Consult & Orderless). Type keywords in any order to find matches, no leading space needed for substring search. Use `C-n`/`C-p` to navigate matches, `RET` to jump. |
| Paste from History | `M-y` | Paste from the kill ring (clipboard) history |
| Go to Line | `M-g g` | Go to a specific line with live preview |
| Kill Buffer | `M-k k` | Open action menu (`M-k`) then press `k` to kill |

## Project Management (Projectile)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Find File in Project | `C-c p f` | Fuzzy search for files in current project |
| Switch Project | `C-c p p` | Switch to another project |
| Search in Project | `C-c p s g` | Grep (search text) in all project files |

## Git (Magit)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Open Magit Status | `M-x magit-status` | The main dashboard. Often bound to `C-x g`. |
| Stage Changes | `s` | In Status buffer: Stage file or hunk at cursor. |
| Unstage Changes | `u` | In Status buffer: Unstage file or hunk at cursor. |
| Commit | `c c` | Start commit. Type message, then `C-c C-c` to finish. |
| Push | `P p` | Push to upstream branch. |
| View History | `l l` | View commit log graph. |
| Refresh | `g` | Refresh the Magit buffer (important as it doesn't auto-refresh). |
| Quit | `q` | Close the Magit window. |

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

### macOS Terminal Clipboard
If you are running Emacs in the terminal (e.g., iTerm2, Ghostty), clipboard integration is automatically handled:
- **Copy (`M-w`)**: Text copied in Emacs is sent to the macOS system clipboard (`pbcopy`).
- **Paste (`C-y`)**: Text pasted in Emacs is pulled from the macOS system clipboard (`pbpaste`).

### Copying from the Messages Buffer
To copy system warnings or error messages:
1.  Switch to the Messages buffer:
    *   Press `C-x b`.
    *   Type `*Messages*` and press `Enter`.
2.  Navigate to the message you want to copy.
3.  Select and copy the text using the Select and Copy steps above.

## Standard Emacs Movement Keys

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

## Window Management

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Shrink Horizontally | `C-c <` | Make window narrower |
| Enlarge Horizontally | `C-c >` | Make window wider |
| Shrink Vertically | `C-c -` | Make window shorter |
| Enlarge Vertically | `C-c +` | Make window taller |
| Mouse Drag | `Left Click + Drag` | Drag status bar or divider to resize (if enabled) |

## Gemini AI (gemini-cli)

Interact with Gemini AI directly from Emacs.

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Start Gemini Chat | `C-c g g` | Start a new chat session (or continue default) |
| Start in Directory | `C-c g d` | Start chat in a specific directory context |
| Send Command | `C-c g s` | Send a one-off command from the minibuffer |
| Send Region | `C-c g r` | Send the selected text (region) to Gemini |
| Send Current File | `C-c g o` | Send the current buffer's file content to Gemini |
| Toggle Window | `C-c g t` | Show/Hide the Gemini chat window |

**In the Chat Buffer:**
*   **`RET`**: Send message.
*   **`C-c C-k`**: Clear chat.
*   **`C-c C-q`**: Quit session.

## Claude AI (claude-code)

Interact with Claude AI (Anthropic) directly from Emacs.

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| Start Claude Chat | `C-c C c` | Start a new chat session |
| Start in Directory | `C-c C d` | Start chat in a specific directory |
| Send Command | `C-c C s` | Send a one-off command from the minibuffer |
| Send Region | `C-c C r` | Send the selected text (region) to Claude |
| Send Current File | `C-c C o` | Send the current buffer's file content to Claude |
| Toggle Window | `C-c C t` | Show/Hide the Claude chat window |

## Configuration Note
Your configuration is in `~/.emacs.d/init.el`.
- API Keys: Stored securely in `secrets.el` (ignored by git).
- Custom Settings: Auto-generated settings (like safe variables) are in `custom.el`.
- Packages: `treemacs` (file tree), `ibuffer` (buffer list), `projectile` (project management), `vertico` (completion), `vterm` (terminal), `markdown-mode`, `consult`, and `marginalia`.
<!--
Local Variables:
buffer-read-only: t
End:
-->