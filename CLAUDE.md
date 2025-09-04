# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TLE (Timmy's Lisp Environment) is a text editor written in Common Lisp, designed for writing Lisp code and general computing tasks. It follows principles of simplicity and minimizing dependencies, similar to Emacs but with a focused scope.

## Development Commands

### Building and Running
- **Quick start**: `./start.sh` - convenience script to start TLE
- **Manual start**: `sbcl --no-userinit --non-interactive --eval '(sb-int:set-floating-point-modes :traps nil)' --eval "(require \"asdf\")" --eval '(asdf:load-system :tle)' --eval '(tle:main)'`
- **REPL mode**: 
  - Load system: `(ql:quickload :tle)` in a Lisp REPL
  - Run application: `(tle:main)` - starts web server on http://localhost:8080
- **Entry point**: `src/main.lisp` contains the main function
- **Port configuration**: Set `TLE_PORT` environment variable to use a specific port (e.g., `TLE_PORT=3000 ./start.sh`). If not set, automatically finds an available port starting from 8080.

### Testing
- **Run tests**: `./test.sh` - runs basic server functionality tests

### Utilities
- **Parenthesis checker**: `util/paren-checker.lisp` - checks for mismatched parentheses in Common Lisp source files
  - **Usage**: `sbcl --load util/paren-checker.lisp --eval '(paren-checker:main "filename.lisp")'`
  - **Programmatic usage**: `(paren-checker:check-parens "code string")` or `(paren-checker:check-parens-file "filename")`
  - **Run tests**: `cd util/tests && sbcl --load test-paren-checker.lisp`

### Dependencies
- **jsown**: JSON parsing library
- **usocket**: Socket library for network communication
- **bordeaux-threads**: Threading library for concurrent operations
- Requires Quicklisp for dependency management

## Architecture

### Core Components (Interface Layer)
- **UI** (`src/ui.lisp`): Abstract UI framework defining generic functions for drawing, events, key handling, and rendering
- **Buffer** (`src/buffer.lisp`): Text buffer abstraction with line-based operations and HTML rendering
- **Editor** (`src/editor.lisp`): Top-level editor state managing buffers and coordinating rendering
- **Frame** (`src/frame.lisp`): Display frame abstraction containing editors (replaces window concept)
- **View** (`src/view.lisp`): Buffer view with cursor position and rendering
- **Application** (`src/application.lisp`): Top-level application managing frames and editors

### Implementation Layer
- **Web-UI** (`src/web-ui.lisp`): Web-based UI implementation using basic HTTP server and WebSockets
- **Standard-*** implementations: Concrete implementations of the abstract interfaces

### Key System Design
- **Generic function-based architecture** with separate interface and implementation layers
- **Hierarchical rendering system**: Application → Frame → Editor → Buffer rendering chain
- **Component-based render methods**: Each component implements `render` and `render-components` methods
- **Frame-Editor architecture**: Frames contain editors (not direct buffer access)
- **Key event system** with modifier support and character mapping
- **Web-based UI** running on localhost:8080 using HTTP server and WebSockets for real-time communication
- **HTML-based text rendering** with line numbers, selection, and cursor display

### File Loading Order (per tle.asd)
1. Package definitions (`tle-package.lisp`, `tle-user-package.lisp`)
2. Interface layer (`ui.lisp`, `buffer.lisp`, `editor.lisp`, `window.lisp`, `view.lisp`)
3. Implementation layer (`web-ui.lisp`, `standard-*.lisp` files)
4. Entry point (`main.lisp`)

## Key Features
- Web-based interface accessible via browser with real-time WebSocket communication
- Comprehensive key binding system with Emacs-style key codes
- Line-based text buffer operations
- Generic UI framework allowing multiple backend implementations
- Self-contained HTTP server without external web framework dependencies

## Window Management System
- **Multi-frame Interface**: Support for multiple draggable windows (frames) within the application
- **Top Menu Bar**: Fixed menu bar with "File" menu containing window management options
- **Window Operations**:
  - **New Window**: Create new frames via "File > New Window" menu
  - **Close Windows**: [X] close buttons in window headers for frame removal
  - **Drag & Drop**: Draggable windows with proper coordinate handling relative to menu bar
  - **Resize**: Window resizing via corner drag handles
- **Z-index Management**: Automatic window stacking with most recently interacted window on top
- **Focus System**: Visual focus indication with lighter header colors for the active window
- **Persistent State**: Window positions, sizes, z-index, and focus state maintained server-side

## Technical Implementation Details
- **Frame System**: Generic frame interface with standard-frame implementation containing editors
- **Render Pipeline**: 
  - `render` methods for complete object rendering
  - `render-components` methods for rendering internal components
  - HTML output with proper line numbering and styling
  - Automatic delegation through the rendering hierarchy
- **Client-Server Communication**: RESTful endpoints for window operations (/frame-new, /frame-close, /frame-update, /frame-zindex, /frame-focus)
- **Real-time Updates**: Server-sent events for immediate UI synchronization across clients
- **CSS Transitions**: Smooth visual feedback for focus changes and interactions
- **Coordinate System**: Menu bar offset handling for proper window positioning
- **Component Architecture**: Clean separation where frames manage editors, editors manage buffers