# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Star-App is a web-based UI for StarIntel, built using CLOG (Common Lisp Omnificent GUI). It provides multiple interfaces for interacting with the StarIntel backend: search, document editing, chat viewing, graph visualization, and target administration.

## Build and Development Commands

### Building with Nix

This project uses Nix flakes for reproducible builds. **Always use Nix to ensure all dependencies are loaded.**

```bash
# Enter development environment (loads SBCL with all dependencies)
nix develop

# Build the executable
nix build

# Run the built executable
./result/bin/star-app
```

### Development in REPL

From within `nix develop`:

```lisp
;; Load the system
(asdf:load-system :star-app)

;; Run the application
(star.app:main)
```

The application will start on port 2233 by default with routes:
- `/search` - Search interface
- `/editor` - Document editor
- `/chat` - Chat/message viewer

### Testing

Tests should be placed in `/t` directory at project root (currently no tests exist).

### Flake Outputs

- `packages.default` - Executable binary `star-app`
- `packages.star-app-lib` - ASDF library package
- `devShells.default` - Development environment with SBCL and all dependencies

## Architecture

### CLOG Framework

This project uses CLOG, a Common Lisp GUI framework that uses WebSockets for real-time client-server communication:

- **Event-driven model**: UI events are handled on the server side
- **WebSocket transport**: After HTTP bootstrap, all communication uses WebSockets
- **Server-side logic**: All application logic runs on the server, browser is just a renderer
- **Parallel execution**: Each CLOG connection runs in its own thread

**Key CLOG concepts**:
- `initialize` - Sets up CLOG server and initial route handler
- `set-on-new-window` - Registers handlers for different paths
- Each page handler receives a `body` object representing the browser window
- Create UI elements with functions like `create-div`, `create-button`, etc.
- Set event handlers with `set-on-click`, `set-on-submit`, etc.

CLOG documentation: `../clog/doc/clog-manual.html` and `../clog/README.md`

### Project Structure

```
source/
├── package.lisp           # Package definition and exports
├── client.lisp            # Star-app class, API client wrapper, JSON utilities
├── utils.lisp            # Utility functions
├── render.lisp           # Document rendering generics, card creation, form generation
├── templates/
│   ├── base.lisp         # Page initialization, CSS loading
│   └── nav.lisp          # Navigation bar component
├── components/
│   └── input-autocomplete.lisp  # Reusable input components
├── pages/
│   ├── search.lisp       # Search interface with FTS
│   ├── editor.lisp       # Document editor with forms
│   ├── chat.lisp         # Message/chat viewer
│   ├── graph.lisp        # Graph visualization
│   └── target-admin.lisp # Target administration
├── run.lisp              # (if exists) Runtime utilities
└── star-app.lisp         # Main entry point, route setup
```

### Application Classes

- `star-app` - Base application class with API client and settings
- `search-app` - Search page state (query, bookmark, host, port)
- `editor-app` - Document editor state (documents, dataset)
- `chat-app` - Chat viewer state (groups, channels, messages)

**Global instances**:
- `*app*` - Main star-app instance
- `*search-app*` - Search page instance
- `*editor-app*` - Editor page instance
- `*chat-app*` - Chat page instance

### Page Initialization Pattern

Each page follows this pattern:

```lisp
(defun on-<page>-page (body)
  (on-page body "Page Title")  ; Loads CSS, sets title, adds navbar
  ;; Create UI elements
  ;; Set up event handlers
  ;; Load initial data
  )
```

### API Communication

- Uses `starintel-gserver-client` for backend communication
- Backend typically at `127.0.0.1:5000` 
- API client accessible via `(api-client *app*)`
- HTTP requests via `dexador`
- JSON parsing via `jsown`

### Styling

- **Spectre.CSS** - Main CSS framework (loaded from CDN)
- **Bootstrap Icons** - Icon library (classes like `bi bi-person-fill`)
- Custom icon mappings in `*dtype-icon-alist*` and `*icon-alist*`

### Document Rendering

Generic functions for rendering different document types:
- `document-header` - Format document title/header
- `document-render-card` - Render full document card
- `document-render-search-result-small` - Compact search result
- `document-render-search-result-large` - Full search result card

Helper functions:
- `create-card` - Create a Spectre.CSS card with title, subtitle, content, chips
- `render-doc-tiles` - Render document fields as key-value tiles

## Dependencies

**Upstream dependency**: `starintel-server` flake at `~/Documents/Projects/starintelV4/starintel-server`
- Provides `starintel-gserver-client` library
- Provides custom SBCL packages (star-cl, cl-couch, cl-gserver)

**Common Lisp libraries** (from nixpkgs.sbclPackages):
- `clog` - GUI framework
- `clack` + `clack-handler-hunchentoot` - Web server
- `hunchentoot` - HTTP server
- `dexador` - HTTP client
- `jsown` - JSON parser
- `log4cl` - Logging
- `str` - String utilities
- `serapeum` - Utility library (used for dict)

**System libraries**:
- openssl, sqlite, lmdb, rabbitmq-c, libffi

## Common Development Patterns

### Creating a New Page

1. Add page file to `source/pages/`
2. Add to `star-app.asd` components list (respecting serial load order)
3. Define page handler function: `(defun on-<name>-page (body) ...)`
4. Register in `main`: `(initialize 'on-<name>-page :port 2233)` and `(set-on-new-window 'on-<name>-page :path "/route")`

### Adding New Document Types

1. Define rendering methods for generics in `render.lisp`:
   - `document-render-search-result-small`
   - `document-render-search-result-large`
2. Add icon mapping to `*dtype-icon-alist*`
3. Add field configuration to `*keys-alist*`
4. Add field icons to `*icon-alist*`

### Working with CLOG Elements

```lisp
;; Create elements
(let ((div (create-div parent :class "container" :style "margin: 10px;")))
  ;; Set content
  (setf (text div) "Hello")
  ;; Set attributes
  (setf (attribute div "data-id") "123")
  ;; Set CSS
  (setf (style div "display") "block")
  ;; Add event handler
  (set-on-click div (lambda (obj) (log:info "Clicked!"))))
```

## Known Issues

## Code Style Notes

- Package: `:star.app`
- Use `in-package` at top of each file
- Event handlers often use `(declare (ignore obj))` when not using the event target
- CLOG elements are created imperatively and assigned to let-bound variables
- Backend responses are JSON parsed with `jsown:parse` and accessed with `jsown:val`
