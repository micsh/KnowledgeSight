# KnowledgeSight

Knowledge and documentation intelligence for any repo. Index your markdown docs, then query, analyze, and maintain them with a composable JS expression language.

## Quick Start

```bash
# Build the index (scans *.md files)
knowledge-sight index --repo /path/to/repo

# Run a semantic search
knowledge-sight search "search('authentication')"

# Health check — orphans, broken links, stale docs
knowledge-sight health --repo /path/to/repo
```

## Installation

Requires .NET 10+.

```bash
dotnet build
```

The tool expects a local embedding server at `http://localhost:1234/v1/embeddings` by default (configurable via `knowledge-sight.json`).

## Commands

| Command | Description |
|---------|-------------|
| `index [--repo <path>]` | Build or incrementally update the doc index |
| `catalog [--repo <path>]` | Show a topic map of all indexed docs |
| `search <expr> [--repo <path>]` | Run a query expression |
| `eval <expr> [--repo <path>]` | Alias for `search` (semantic clarity for non-search expressions) |
| `orphans [--repo <path>]` | Find docs with no incoming links |
| `broken [--repo <path>]` | Find broken links across docs |
| `stale [--repo <path>]` | Find docs drifting from source code |
| `health [--repo <path>]` | All checks: orphans + broken + stale + density |
| `check <text\|file> [--repo <path>] [--expr]` | Detect novel knowledge in text or a file; `--expr` for JS expression mode |
| `fn add <name> <body> [opts]` | Define a reusable function |
| `fn list [--repo <path>]` | List saved functions |
| `fn rm <name> [--repo <path>]` | Remove a function |
| `--help` | Show help |

## Query Language

Queries are JavaScript expressions evaluated by [Jint](https://github.com/sebastienros/jint). All primitives are available as globals and can be composed freely.

```bash
# Simple search
knowledge-sight search "search('auth')"

# Chain: search then expand the top result
knowledge-sight search "search('auth'); expand(R1)"

# Multi-step: find a file's context, then walk its link graph
knowledge-sight search "context('docs/architecture.md')"
knowledge-sight search "walk('docs/architecture.md', {depth: 3})"

# Combine results
knowledge-sight search "search('auth').concat(grep('JWT'))"
```

### Composition Helpers

In addition to the query primitives, four composition helpers are available as globals:

| Helper | Description |
|--------|-------------|
| `pipe(value, fn1, fn2, ...)` | Thread a value through a series of functions sequentially |
| `tap(value, fn)` | Run `fn` for side-effects (e.g. debugging), return `value` unchanged |
| `mergeBy(key, arr1, arr2, ...)` | Union multiple arrays with dedup by the specified key field |
| `print(value)` | Debug output to stderr (Jint has no `console` object — use `print` instead) |

```js
// Thread search results through similar then expand
pipe(search('auth'), function(r) { return similar(r[0].id) })

// Debug without breaking chains
tap(search('auth'), function(r) { print('found ' + r.length + ' results') })

// Combine results from different primitives with dedup
mergeBy('id', search('auth'), grep('authentication'))
```

### Primitives

| Primitive | Description |
|-----------|-------------|
| `search(query, {limit, tag, file})` | Semantic search across indexed chunks |
| `catalog()` | Topic map of all indexed docs |
| `context(file)` | Overview of a file with sections, backlinks, and outlinks |
| `expand(refId)` | Expand an `R#` ref to full chunk content |
| `neighborhood(refId, {before, after})` | Surrounding sections around a ref |
| `similar(refId, {limit})` | Semantically similar chunks |
| `grep(pattern, {limit, file})` | Regex search over chunk content |
| `mentions(term, {limit})` | Find term mentions across docs |
| `files(pattern)` | List indexed files |
| `backlinks(file)` | Incoming links to a file |
| `links(file)` | Outgoing links from a file |
| `orphans()` | Docs with no incoming links |
| `broken()` | Broken links across docs |
| `placement(content, {limit})` | Suggest where new content fits |
| `walk(file, {depth, direction})` | Traverse the link graph |
| `novelty(text, {threshold})` | Detect novel knowledge in text |
| `cluster(dir, {threshold})` | Cluster docs by similarity |
| `gaps({scope, min_docs, signal})` | Find coverage gaps |

### Refs

Search results are assigned ref IDs (`R1`, `R2`, ...) that persist across queries. Use `expand(R1)` or `similar(R1)` to drill into previous results.

## Functions

Save chains of primitives as reusable, named functions. Functions are **per-repo** — each repo has its own set, stored in `knowledge-sight.functions.json` at the repo root.

### Defining Functions

```bash
# Simple function with no parameters
knowledge-sight fn add overview --desc "Quick topic map" "catalog()"

# Function with parameters
knowledge-sight fn add deepSearch --params "q" --desc "Search + similar expansion" \
  "search(q).concat(similar(search(q)[0]))"

# Multi-parameter function
knowledge-sight fn add scopedGrep --params "pattern,file" \
  "grep(pattern, {file: file})"

# Read body from a file (useful for complex functions)
knowledge-sight fn add audit --file audit-fn.js --desc "Full doc audit"
```

### Using Functions

Once defined, functions are available in any `search` expression:

```bash
knowledge-sight search "deepSearch('authentication')"
knowledge-sight search "scopedGrep('TODO', 'docs/roadmap.md')"
```

### Managing Functions

```bash
# List all functions (compact: name, params, description)
knowledge-sight fn list

# Full details including function body
knowledge-sight fn list --verbose

# Machine-readable JSON output
knowledge-sight fn list --json

# Remove a function
knowledge-sight fn rm deepSearch
```

### Options for `fn add`

| Option | Description |
|--------|-------------|
| `--params "a,b"` | Comma-separated parameter names |
| `--desc "..."` | Optional description |
| `--file <path>` | Read function body from a file instead of inline |

### Storage

Functions are stored in `knowledge-sight.functions.json` in the repo root. This file is meant to be committed alongside your docs so the whole team shares the same functions.

> **Note:** Functions are available in `search` and `eval` expressions. The `check` command also supports UDFs when invoked with `--expr` (see below).

## The `eval` Command

`eval` is an alias for `search`. Use it when your expression isn't really a "search" — e.g. `eval "orphans().concat(broken())"`. Behavior is identical.

## Check with `--expr`

By default, `check` runs the plain-text novelty pipeline. The `--expr` flag switches to JS expression mode with full access to primitives and user-defined functions:

```bash
# Plain text — novelty classification (default)
knowledge-sight check "The stanza parser must handle malformed XML."

# Expression mode — full QueryEngine with UDFs
knowledge-sight check "novelty('stanza parser handles malformed XML').filter(function(r) { return r.status === 'novel' })" --expr
```

## Improved Result Formatting

Mixed result arrays (e.g. `orphans().concat(broken())`) now format cleanly. Each result item carries internal type metadata, so the formatter applies deterministic per-item formatting instead of guessing the shape of the entire array. No action needed — this is automatic.

## Configuration

Create a `knowledge-sight.json` in the repo root to customize behavior:

```json
{
  "docDirs": ["docs", "wiki"],
  "exclude": ["node_modules", "bin", "obj", ".git"],
  "indexDir": ".knowledge-sight",
  "embeddingUrl": "http://localhost:1234/v1/embeddings",
  "embeddingBatchSize": 50
}
```

All fields are optional. Defaults:

| Field | Default |
|-------|---------|
| `docDirs` | Auto-detected from `.agents`, `docs`, `doc`, `wiki`, `knowledge`, or `.` |
| `exclude` | `node_modules`, `bin`, `obj`, `.git`, `wwwroot`, `dist`, `.code-intel` |
| `indexDir` | `.knowledge-sight` |
| `embeddingUrl` | `http://localhost:1234/v1/embeddings` |
| `embeddingBatchSize` | `50` |

## Frontmatter

Docs can include YAML frontmatter with a `related` field to link docs to source files. The `stale` and `health` commands use this to detect when source has changed since the doc was last updated.

```markdown
---
related:
  - src/Auth/TokenService.fs
  - src/Auth/LoginHandler.fs
---
```
