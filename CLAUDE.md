# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

TLE (Timmy's Lisp Environment) is a text editor written in Common Lisp, designed for writing Lisp code and general computing tasks. It follows principles of simplicity and minimizing dependencies, similar to Emacs but with a focused scope.

## Development Commands

### Building and Running
- **Quick start**: `./start.sh` - convenience script to start TLE
- **Manual start**: `sbcl --non-interactive --eval '(ql:quickload :tle)' --eval '(tle:main)'`
- **REPL mode**: 
  - Load system: `(ql:quickload :tle)` in a Lisp REPL
  - Run application: `(tle:main)` - starts web server on http://localhost:8080
- **Entry point**: `src/main.lisp` contains the main function

### Testing
- **Run tests**: `./test.sh` - runs basic server functionality tests

### Dependencies
- **jsown**: JSON parsing library
- **usocket**: Socket library for network communication
- **bordeaux-threads**: Threading library for concurrent operations
- Requires Quicklisp for dependency management

## Architecture

### Core Components (Interface Layer)
- **UI** (`src/ui.lisp`): Abstract UI framework defining generic functions for drawing, events, and key handling
- **Buffer** (`src/buffer.lisp`): Text buffer abstraction with line-based operations
- **Editor** (`src/editor.lisp`): Top-level editor state managing buffers and windows
- **Window** (`src/window.lisp`): Display window abstraction
- **View** (`src/view.lisp`): Buffer view with cursor position and rendering

### Implementation Layer
- **Web-UI** (`src/web-ui.lisp`): Web-based UI implementation using basic HTTP server and WebSockets
- **Standard-*** files: Concrete implementations of the abstract interfaces

### Key System Design
- Generic function-based architecture with separate interface and implementation layers
- Key event system with modifier support and character mapping
- Web-based UI running on localhost:8080 using HTTP server and WebSockets for real-time communication
- Text rendering with selection and cursor display

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