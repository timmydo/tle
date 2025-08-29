# TODO: Missing Editor Functions for TLE Buffer

## Current Implementation Status
The buffer.lisp currently implements:
- Basic cursor movement (forward-char, backward-char, next-line, previous-line)
- Character and newline insertion
- Point and mark management
- Basic selection rendering

## Missing Standard Editor Functions

### 1. Text Deletion
- [x] `delete-char` - Delete character at point
- [x] `delete-backward-char` (backspace) - Delete character before point
- [x] `kill-line` - Delete from point to end of line
- [x] `kill-whole-line` - Delete entire current line
- [x] `kill-word` - Delete word forward
- [x] `backward-kill-word` - Delete word backward
- [x] `delete-region` - Delete text between mark and point

### 2. Word Movement
- [x] `forward-word` - Move point forward by one word
- [x] `backward-word` - Move point backward by one word
- [x] `beginning-of-word` - Move to beginning of current word
- [x] `end-of-word` - Move to end of current word

### 3. Line Movement
- [x] `beginning-of-line` - Move point to start of line
- [x] `end-of-line` - Move point to end of line
- [x] `move-beginning-of-line` - Smart beginning of line (after indentation)

### 4. Buffer Movement
- [x] `beginning-of-buffer` - Move point to start of buffer
- [x] `end-of-buffer` - Move point to end of buffer
- [x] `goto-line` - Jump to specific line number

### 5. Copy/Cut/Paste (Kill Ring)
- [x] `kill-region` - Cut text between mark and point to kill ring
- [x] `copy-region-as-kill` - Copy text between mark and point to kill ring
- [x] `yank` - Paste from kill ring
- [x] `yank-pop` - Cycle through kill ring entries
- [x] Kill ring data structure and management

### 6. Search and Replace
- [x] `search-forward` - Search for text forward
- [x] `search-backward` - Search for text backward
- [x] `isearch-forward` - Incremental search forward
- [x] `isearch-backward` - Incremental search backward
- [x] `query-replace` - Interactive find and replace
- [ ] `replace-string` - Replace all occurrences

### 7. Text Transformation
- [ ] `upcase-word` - Convert word to uppercase
- [ ] `downcase-word` - Convert word to lowercase
- [ ] `capitalize-word` - Capitalize word
- [ ] `upcase-region` - Convert region to uppercase
- [ ] `downcase-region` - Convert region to lowercase
- [ ] `transpose-chars` - Swap characters around point
- [ ] `transpose-words` - Swap words around point
- [ ] `transpose-lines` - Swap current line with previous

### 8. Indentation
- [ ] `indent-line` - Indent current line
- [ ] `indent-region` - Indent all lines in region
- [ ] `tab-to-tab-stop` - Insert tab or spaces to next tab stop
- [ ] `delete-indentation` - Join current line with previous
- [ ] Auto-indentation based on mode

### 9. File Operations
- [x] `save-buffer` - Save buffer to file
- [x] `save-buffer-as` - Save buffer to new file
- [x] `load-file` - Load file into buffer
- [ ] `revert-buffer` - Reload buffer from file
- [ ] `write-file` - Write buffer to file
- [ ] File modification tracking (dirty flag)

### 10. Selection and Marking
- [ ] `select-all` - Mark entire buffer
- [ ] `exchange-point-and-mark` - Swap point and mark positions
- [ ] `mark-word` - Mark current word
- [ ] `mark-paragraph` - Mark current paragraph
- [ ] `mark-whole-buffer` - Mark entire buffer

### 11. Scrolling and Display
- [ ] `scroll-up` - Scroll view up (show later text)
- [ ] `scroll-down` - Scroll view down (show earlier text)
- [ ] `recenter` - Center point in window
- [ ] `scroll-to-line` - Scroll to make line visible

### 12. Multi-line Operations
- [ ] `open-line` - Insert newline without moving point
- [ ] `split-line` - Split line at point
- [ ] `join-line` - Join current line with next
- [ ] `duplicate-line` - Duplicate current line

### 13. Rectangle Operations
- [ ] `kill-rectangle` - Cut rectangular region
- [ ] `copy-rectangle-as-kill` - Copy rectangular region
- [ ] `yank-rectangle` - Paste rectangular region
- [ ] `clear-rectangle` - Clear rectangular region
- [ ] `string-rectangle` - Replace rectangle with string

### 14. Parentheses and Bracket Matching
- [ ] `forward-sexp` - Move forward by s-expression
- [ ] `backward-sexp` - Move backward by s-expression
- [ ] `up-list` - Move up one level of parentheses
- [ ] `down-list` - Move down one level of parentheses
- [ ] `check-parens` - Check parentheses balance
- [ ] Highlight matching parentheses

### 15. Comment Operations
- [ ] `comment-line` - Comment/uncomment current line
- [ ] `comment-region` - Comment/uncomment region
- [ ] `comment-dwim` - Do what I mean with comments

### 16. Registers
- [ ] `point-to-register` - Save point position to register
- [ ] `jump-to-register` - Jump to position in register
- [ ] `copy-to-register` - Copy region to register
- [ ] `insert-register` - Insert register contents

### 17. Bookmarks
- [ ] `bookmark-set` - Set bookmark at current position
- [ ] `bookmark-jump` - Jump to bookmark
- [ ] `bookmark-list` - List all bookmarks
- [ ] `bookmark-delete` - Delete bookmark

### 18. Advanced Editing
- [ ] `fill-paragraph` - Wrap paragraph to fill column
- [ ] `fill-region` - Wrap region to fill column
- [ ] `sort-lines` - Sort lines in region
- [ ] `reverse-region` - Reverse order of lines in region
- [ ] `count-words-region` - Count words in region

### 19. Buffer State Management
- [ ] Multiple undo/redo with history tree
- [ ] Change tracking and modification timestamps
- [ ] Buffer-local variables
- [ ] Read-only buffer support
- [ ] Buffer encoding (UTF-8, etc.)

### 20. Performance and Memory
- [ ] Gap buffer or rope data structure for efficient editing
- [ ] Lazy loading for large files
- [ ] Syntax highlighting integration
- [ ] Line ending normalization (LF, CRLF, CR)

## Priority Levels

### High Priority (Core Functionality)
- Text deletion functions (delete-char, kill-line, etc.)
- Word movement (forward-word, backward-word)
- Line movement (beginning-of-line, end-of-line)
- Copy/cut/paste operations
- File operations (save, load)

### Medium Priority (Standard Features)
- Search and replace
- Text transformation
- Indentation
- Multi-line operations
- Undo/redo improvements

### Low Priority (Advanced Features)
- Rectangle operations
- Registers and bookmarks
- Advanced text processing
- Performance optimizations

## Implementation Notes

1. Many functions should operate on regions when mark is active
2. All editing operations should properly update undo history
3. Functions should handle edge cases (empty buffer, end of file, etc.)
4. Consider key binding integration for each function
5. Error handling for invalid operations
6. Integration with the existing point/mark system
7. Proper HTML rendering updates for new features