# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Build and Development
```bash
# Create dist directory and watch for changes
mkdir dist
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }

# Serve the application locally
deno run -A npm:serve dist -s -C -S -n
```

### Elm Compilation
```bash
# Compile Elm application for development (with debug)
elm make src/Main.elm --debug --output=dist/index.js

# Compile for production (optimized)
elm make src/Main.elm --optimize --output=dist/index.js
```

## Architecture Overview

This is "diggit" - a Git repository visualization and analysis tool built in Elm that runs in the browser. The application title shows as "scrapsheets" in the UI.

### Core Architecture
- **Frontend**: Single Elm application (src/Main.elm) following The Elm Architecture (TEA)
- **Git Integration**: Browser-based Git operations using isomorphic-git and LightningFS
- **Data Flow**: Ports for JavaScript interop to handle Git operations and file system access
- **AI Integration**: Claude API integration for repository analysis and reporting

### Key Data Models
- **Event**: Core data structure representing commits, branches, issues, and other repository events
- **Repo**: Complete repository data including commits, authors, branches, files, and GitHub data
- **Filters**: User interface state for filtering and searching repository data
- **Report**: AI-generated analysis and suggestions for repository insights

### JavaScript Integration
The application heavily relies on JavaScript ports for:
- Git repository cloning and analysis using isomorphic-git
- File system operations using LightningFS (browser-based filesystem)
- Claude API calls for AI-powered repository analysis
- Progress reporting during long-running operations

### Main Components
- Repository loading and Git operations (index.html JavaScript)
- Event filtering and visualization (Main.elm)
- Claude AI integration for repository insights
- Tag-based filtering system for repository events

### Data Flow
1. User inputs repository URL (GitHub format: owner/repo)
2. JavaScript clones repository using isomorphic-git
3. Extracts commits, authors, branches, and file changes
4. Sends data to Elm through ports
5. Elm processes and visualizes the data
6. Claude integration provides AI-powered insights

The application is designed to work entirely in the browser without backend dependencies.