# Emacs Basics

## Essential Commands

| Action | Key Sequence | Mnemonic |
| :--- | :--- | :--- |
| **Quit Emacs** | `C-x C-c` | **C**lose / Exit |
| **Open/Create File** | `C-x C-f` | **F**ind file |
| **Save File** | `C-x C-s` | **S**ave |
| **Cancel Command** | `C-g` | **G**et me out! (Stop current action) |

## File Explorer (Treemacs)

| Action | Key Sequence | Note |
| :--- | :--- | :--- |
| **Toggle Sidebar** | `C-x t t` | Open/Close the file tree |
| **Delete Other Windows** | `C-x t 1` | Maximize the current window, hiding others |

## Copying & Pasting

### General Copy/Paste
1.  **Select Text:**
    *   Move cursor to the start of the text.
    *   Press `C-SPC` (Control + Space) to "Set Mark".
    *   Move cursor to the end of the text to highlight it.
2.  **Copy:**
    *   Press `M-w` (Meta + w). *Note: On Mac, Meta is usually the Option key.*
3.  **Paste (Yank):**
    *   Press `C-y` to paste.

### Copying from the Messages Buffer
To copy system warnings or error messages:
1.  Switch to the Messages buffer:
    *   Press `C-x b`.
    *   Type `*Messages*` and press `Enter`.
2.  Navigate to the message you want to copy.
3.  Select and copy the text using the **Select** and **Copy** steps above.

## Configuration Note
Your configuration has been moved to `~/.emacs.d/init.el`.
The `treemacs` package is configured to install automatically from MELPA on the next startup.