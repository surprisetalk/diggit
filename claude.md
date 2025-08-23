# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Quick Start
```bash
# Build and serve for development
mkdir -p dist
cp src/* dist
elm make src/Main.elm --debug --output=dist/index.js
npx serve dist -s -C -S -n
```

### Build Commands
```bash
# Development build (with debug)
elm make src/Main.elm --debug --output=dist/index.js

# Production build (optimized)
elm make src/Main.elm --optimize --output=dist/index.js

# Copy static assets to dist
cp src/* dist
```

### Development Workflow
```bash
# Watch for changes and rebuild (macOS/BSD)
fswatch -o src/ | while read f; do cp src/* dist && elm make src/Main.elm --debug --output=dist/index.js; done

# Alternative watch command
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }

# Serve locally (choose one)
npx serve dist -s -C -S -n        # Using npm serve
deno run -A npm:serve dist -s -C -S -n  # Using Deno
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
The application uses Elm ports for JavaScript interop (defined in index.html):

**Outgoing Ports (Elm → JavaScript):**
- `loadRepo`: Initiate repository cloning and analysis
- `runReport`: Execute Claude API analysis
- `deleteRepos`: Clear browser filesystem
- `openUrl`: Open external URLs
- `saveRepoToLocalStorage`: Persist repository data
- `loadRepoFromLocalStorage`: Retrieve cached repository data

**Incoming Ports (JavaScript → Elm):**
- `repoData`: Receive processed repository data
- `cacheKey`: Provide cache identifiers
- `reportOutput`: Return Claude API responses
- `progress`: Report operation progress

Key JavaScript libraries loaded via CDN:
- `isomorphic-git`: Git operations in browser
- `LightningFS`: Virtual filesystem for Git repos
- CORS proxy: `https://cors.isomorphic-git.org`

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

### Key Elm Messages
Important message types in Main.elm:
- `LoadRepo String`: Load repository from GitHub URL
- `ReceivedRepoData`: Process incoming repository data
- `ReceivedProgress`: Update progress indicators
- `RunReport`: Initiate Claude analysis
- `ReceivedReportOutput`: Process Claude response
- `ToggleFilter`: Handle event filtering
- `ToggleTag`: Tag-based filtering

The application is designed to work entirely in the browser without backend dependencies.