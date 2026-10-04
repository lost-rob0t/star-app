# Star-App

**Status: Experimental / Work in Progress**

A web-based UI for StarIntel, built using CLOG (Common Lisp Omnificent GUI). This application provides multiple interfaces for interacting with the StarIntel backend including search, document editing, chat viewing, graph visualization, and target administration.

⚠️ **This is experimental software under active development. Features may be incomplete, buggy, or subject to change.**

## Features

- **Search Interface** (`/search`) - Full-text search with autocomplete
- **Document Editor** (`/editor`) - Edit and manage documents
- **Chat Viewer** (`/chat`) - View messages and conversations
- **Graph Visualization** - Unregistered prototype
- **Target Administration** - Unregistered prototype

## Document contract

StarLang is the sole source of truth for document fields and validation. This consumer vendors the complete generated 0.10.1 release at immutable commit `765f1673851192bcaf1cdd2f35c47608f979b079`. Portable Nix inputs pin the server and maintained star-cl runtime; checks compare every release artifact across the application, actual CL runtime, and server client.

The UI instantiates the actual generated Common Lisp structs through `starintel.canonical`, with presentation wrappers for CLOG rendering. Form fields and available document types come from the generated schema, rather than a local model package. Canonical `id`, `schemaVersion`, `createdAt`, `updatedAt`, Message `message`, and qualified reference objects are used throughout. False, null, empty arrays, decimal strings, omitted optional fields, and opaque extensions survive roundtrips. Invalid documents are rejected before HTTP submission.

The editor validates and retains documents in its existing local collection; adding a card does not persist a document to the backend. `submit-canonical-document` uses the maintained HTTP client and validated canonical JSON when explicit application code submits a document. The graph prototype remains unregistered, and chat remains a viewer.

Full Nix validation builds the entire library and saved executable, exercises all 60 generated document types and typed forms against a real HTTP endpoint, and launches the installed executable in Chromium over actual CLOG WebSockets. Browser checks cover all source-defined form fields, rejected/valid editor submissions, exact false/null/empty extension values, canonical chat references/content, and search. The fixture runs offline and does not assess CDN styling. The Nix dependency source contains a narrow SBCL readtable-iterator compatibility patch; it tests macro characters through the public accessor and preserves the runtime source used by ASDF.

## Prerequisites

- Nix with flakes enabled
- StarIntel backend running (typically at `127.0.0.1:5000`)

## Running the Application

### Quick Start

```bash
# Enter the Nix development environment
nix develop

# Start SBCL REPL
sbcl

# In the REPL, load and run the application
(asdf:load-system :star-app)
(star.app:main)
```

The application will start on **port 2233** and open a browser when `STAR_APP_OPEN_BROWSER` is set.

Access the application at:
- http://localhost:2233/search
- http://localhost:2233/editor
- http://localhost:2233/chat

### One-Liner Start

```bash
nix develop -c sbcl --eval "(asdf:load-system :star-app)" --eval "(star.app:main)"
```

### Building the Executable

```bash
# Build with Nix
nix build

# Run the built executable
./result/bin/star-app
```

## Development

### File Structure

```
source/
├── package.lisp              # Package definition and exports
├── documents.lisp            # Presentation wrappers over generated bindings
├── contract.lisp             # Source-defined fields and typed form projection
├── client.lisp               # Maintained HTTP client and canonical codec
├── utils.lisp               # Utility functions
├── render.lisp              # Document rendering generics
├── templates/
│   ├── base.lisp            # Page initialization, CSS loading
│   └── nav.lisp             # Navigation bar component
├── components/
│   └── input-autocomplete.lisp  # Reusable input components
├── pages/
│   ├── search.lisp          # Search interface
│   ├── editor.lisp          # Document editor
│   ├── chat.lisp            # Message/chat viewer
│   ├── graph.lisp           # Graph visualization
│   └── target-admin.lisp    # Target administration
├── run.lisp                 # Runtime utilities
└── star-app.lisp            # Main entry point, route setup
```

### Reloading Changes

When making changes to the code, reload in the REPL:

```lisp
;; Force reload all code
(asdf:load-system :star-app :force t)

;; Restart the application
(star.app:main)
```

Note: You may need to kill the old process first if port 2233 is already in use.

### Running Tests

Tests should be placed in the `/t` directory at project root.

```bash
python3 scripts/sync-starintel-schema.py
nix flake check -L
```

## Dependencies

This project uses Nix flakes for reproducible builds. All dependencies are managed through the flake.

### Common Lisp Libraries
- `clog` - GUI framework
- `clack` + `clack-handler-hunchentoot` - Web server
- `hunchentoot` - HTTP server
- `dexador` - HTTP client
- `com.inuoe.jzon` - Lossless canonical JSON parser
- `starintel-0101` - StarLang-generated document bindings and validator
- `jsown` - Existing graph presentation helpers
- `log4cl` - Logging
- `str` - String utilities
- `serapeum` - Utility library
- `starintel-gserver-client` - StarIntel backend client

### System Libraries
- openssl, sqlite, lmdb, rabbitmq-c, libffi

## Architecture

Star-App uses CLOG's event-driven model where:
- All UI logic runs on the server side
- The browser is just a renderer
- WebSocket transport handles real-time communication
- Each CLOG connection runs in its own thread

See [CLAUDE.md](./CLAUDE.md) for Claude created summery.


## Configuration

The application expects the StarIntel backend at:
- `127.0.0.1:5000` (default local)

Set `STARINTEL_API_URL` at launch to choose the backend and `STARINTEL_API_KEY` to use an API credential. `STAR_APP_PORT` selects the UI port; `STAR_APP_OPEN_BROWSER` opts into opening a browser. Configuration is applied at launch, including in the saved executable.

## Troubleshooting

### Port Already in Use

If port 2233 is already in use, kill the existing process:

```bash
# Find the process
ss -lptn 'sport = :2233'

# Kill it
kill -9 <PID>
```

### Static Files Not Loading

The installed application serves the pinned CLOG bootstrap and JavaScript assets. Presentation styles are loaded from the existing CDN URLs.

### WebSocket Connection Issues

Check that:
1. The CLOG server is running
2. No firewall is blocking port 2233
3. Browser console for WebSocket errors

## Contributing

This is an experimental project. Code contributions should:
- Follow existing code style
- Be tested locally before submission
- Update documentation as needed

## License

GPLv3
