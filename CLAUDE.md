# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Build and Development
```bash
# Build the project
dune build

# Run all tests
dune test

# Run tests and see output
dune runtest

# Execute the main tool
dune exec -- odoc_notebook [command] [args]

# Execute the test tool
dune exec -- odoc_notebook_test [args]

# Format OCaml code (uses OCamlFormat v0.27.0)
ocamlformat -i file.ml

# Run a single test file
dune exec -- dune test test/simple.t
```

### Common Usage Examples
```bash
# Prepare OPAM packages for web deployment
dune exec -- odoc_notebook opam astring base

# Generate an interactive notebook from a .mld file
dune exec -- odoc_notebook generate file.mld --odoc-dir _tmp/_odoc

# Test a notebook file
dune exec -- odoc_notebook_test file.mld
```

## Architecture Overview

### Core Components

1. **Code Block Parsing** (`lib/blocks.ml`)
   - Extracts code blocks from `.mld` files with metadata
   - Supports multiple block types: `{@ocaml}`, `{@ocamltop}`, `{@ocaml deferred-js}`
   - Handles directives like `@libs` for loading dependencies

2. **Notebook Generation Pipeline**
   - Input: `.mld` or `.md` files with annotated code blocks
   - Processing: `odoc` compilation → HTML generation → JavaScript injection
   - Output: Static HTML with embedded OCaml toplevel worker

3. **Worker Architecture** (`top_worker/`)
   - Web worker-based OCaml toplevel execution
   - Isolated execution environment in browser
   - Communication via structured messages between frontend and worker

4. **Frontend Integration** (`frontend/`)
   - Handles user interactions with code cells
   - Manages worker communication
   - Renders execution results with MIME type support

### Key Design Patterns

1. **Eio-based Concurrency**
   - Uses Eio for effects-based I/O throughout CLI tools
   - Worker pool implementation for parallel processing
   - Fiber-based task management

2. **Two-Stage Compilation**
   - Stage 1: Extract and compile OCaml code blocks with js_of_ocaml
   - Stage 2: Generate HTML with pre-compiled JavaScript modules

3. **Library Management**
   - OPAM packages are pre-compiled to JavaScript
   - Dynamic loading in browser via `Toploop_ext.add_unit`
   - Dependency resolution handled during generation phase

### Testing Strategy

Tests use cram test format (`.t` files) with expected output validation:
- Located in `test/*/run.t`
- Test both successful execution and error cases
- Verify HTML output structure and JavaScript generation

### Important Context

- The project bridges OCaml documentation (odoc) with interactive notebooks
- All OCaml code execution happens client-side in the browser
- The tool generates completely static HTML files - no server required
- Supports rich output via MIME types (text, images, interactive visualizations)
- Integrates with OCaml web libraries like Brr for browser programming