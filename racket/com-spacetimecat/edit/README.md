# Racket Editor

I think, because there is already a better editor somewhere else,
I should focus more on making an analyzer than on making an editor.

Features:

- Navigate: Quick Open File with fuzzy path matching like Visual Studio Code Ctrl+P
- Outliner: Hierarchical outline view

Wish list:

- Control: Eval window
- Navigate: Define word under cursor
- Edit: Remember last editor state (open files, layout, etc.)
- Navigate: History, vi C-i, C-o
- Navigate: Scrolling, vi C-f, C-b
- Navigate: Word, vi w, e, b, W, E, B, *, #, n, N
- Navigate: Line, vi 0, $, (line)G, gg, G
- Search: Find string in files like grep
- Analyze: Find who uses this
- Analyze: Find who is used by this
- Edit: Quick Insert: Ctrl+Space
- View: File/line position indicator
- Edit: Go-to matching bracket
- Edit: Highlight matching bracket
- Edit: Remove trailing spaces on save
- Edit: Pressing Tab should insert spaces
- Edit: Indent new line based on the indentation of the previous line and the depth of previous bracket
- Edit: Format (4 spaces indent, so that ugly programs hurt more)
- Analyze: Call graph
- Analyze: Data flow
- Refactor: Rename element
- Refactor: Rename file

Unlikely to be implemented:

- Edit: Auto-save and auto-backup
- Edit: Syntax highlighting
- Edit: Styles, colors, appearances, etc.
- Git integration
- Diff
