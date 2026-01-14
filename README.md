# Star-App

**Status: Experimental / Work in Progress**

A web-based UI for StarIntel, built using CLOG (Common Lisp Omnificent GUI). This application provides multiple interfaces for interacting with the StarIntel backend including search, document editing, chat viewing, graph visualization, and target administration.

⚠️ **This is experimental software under active development. Features may be incomplete, buggy, or subject to change.**

## Features

- **Search Interface** (`/search`) - Full-text search with autocomplete
- **Document Editor** (`/editor`) - Edit and manage documents
- **Chat Viewer** (`/chat`) - View messages and conversations
- **Graph Visualization** (`/graph`) - Network graph visualization
- **Target Administration** (`/target-admin`) - Manage targets

## Theme

The application features a dark electric neon synthwave theme with:
- Deep dark blue/black gradients
- Neon cyan and pink accents with glow effects
- Purple glowing cards and borders
- Smooth transitions and hover animations

## Prerequisites

- Nix with flakes enabled
- StarIntel backend running (typically at `127.0.0.1:5000` or `database.star.intel:5000`)

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

The application will start on **port 2233** and automatically open your browser.

Access the application at:
- http://localhost:2233/search
- http://localhost:2233/editor
- http://localhost:2233/chat
- http://localhost:2233/graph
- http://localhost:2233/target-admin

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
├── client.lisp               # API client wrapper, JSON utilities
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

static/
└── css/
    └── synthwave.css        # Custom synthwave theme
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
# TODO: No tests currently exist
```

## Dependencies

This project uses Nix flakes for reproducible builds. All dependencies are managed through the flake.

### Common Lisp Libraries
- `clog` - GUI framework
- `clack` + `clack-handler-hunchentoot` - Web server
- `hunchentoot` - HTTP server
- `dexador` - HTTP client
- `jsown` - JSON parser
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

See [CLAUDE.md](./CLAUDE.md) for detailed architecture documentation.

## Known Issues

See [CLEANUP.md](./CLEANUP.md) for a detailed list of known issues and TODOs.

## Configuration

The application expects the StarIntel backend at:
- `127.0.0.1:5000` (default local)
- `database.star.intel:5000` (remote)

Modify `source/client.lisp` to change backend endpoints.

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

Ensure the `static/` directory exists and contains the CSS files. The static root is configured in `source/star-app.lisp`.

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

[License information to be added]

## References

- [CLOG Documentation](../clog/README.md)
- [CLOG Manual](../clog/doc/clog-manual.html)
- [Project Instructions](./CLAUDE.md)
- [Cleanup Tasks](./CLEANUP.md)
