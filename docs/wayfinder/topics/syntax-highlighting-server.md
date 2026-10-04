# Syntax highlighting server

A design for precise, incremental syntax highlighting for `quill`, built as a
long-running compiler server with a thin editor client. The server does all
language-specific work; the client only applies spans. The same server targets
every editor.

This sharpens [the syntax-highlighting ticket](../tickets/syntax-highlighting.md):
the consumer is neovim, the "good enough static grammar" trade-off is rejected
in favour of compiler-driven precision, and the first slice of the
[first-class compiler API](first-class-elaborator-api.md) is the server's tree
interface.

## Goals

- **Precise** — operators, macros, and binding roles come from the elaborator,
  not from a context-free grammar. A variable named `+` is not mis-coloured.
- **Fast** — a long-running server holds the elaborated prelude; editing a
  buffer re-elaborates only that buffer, not the project.
- **Incremental** — on edit, only affected definitions are re-elaborated;
  unchanged subtrees keep their identity.
- **Multi-editor** — neovim-only for now. The server is editor-agnostic
  (msgpack-RPC), so other editors are a thin client later. The Lua client is
  ~50 lines and contains no language-specific logic.

## Architecture

```
┌─────────────┐   msgpack-RPC over stdio   ┌──────────────────────┐
│  editor      │ ◄────────────────────────► │  quill-highlight server │
│  (thin)      │   did_open / did_change    │  (.NET, long-running) │
│              │ ◄────────────────────────  │                      │
│              │   highlight spans           │  reader → enforester │
└─────────────┘                             │  → elaborator → tree │
                                            │  → query → spans     │
                                            └──────────────────────┘
```

The server is a standalone process. The editor spawns it, sends buffer text,
receives highlight spans. The editor never parses, never resolves bindings,
never understands quill's grammar.

## Why tree-sitter's pattern, not tree-sitter's API

Tree-sitter's design — **source → incremental parser → persistent tree →
declarative query → highlights** — is the right shape. Its C API is not the
right boundary for a .NET compiler (see the
[syntax-highlighting ticket](../tickets/syntax-highlighting.md) for the FFI
analysis). So we mimic the pattern:

- **Persistent tree** — the server maintains an elaborated syntax tree per
  buffer, edited in place on change.
- **Declarative query** — highlighting rules are S-expression patterns over
  the tree, not logic embedded in the client.
- **Incremental** — the tree is updated only where the source changed.

The difference from tree-sitter: the tree is **semantic**, not just syntactic.
Binding roles, macro expansions, and operator declarations are already resolved
by the elaborator. The query layer reads roles; it does not re-derive them.

## Server design

### Tree

Each open buffer has an elaborated tree — a flat node list:

```lua
{ id = 7, type = "call", role = "function",
  span = { start_byte = 42, end_byte = 50, start_line = 3, start_col = 5, end_line = 3, end_col = 13 },
  children = { 8, 9, 10 } }
```

- `type` — syntactic form: `function_definition`, `call`, `match`,
  `identifier`, `operator`, `string`, `comment`, …
- `role` — semantic role from the binding table: `variable`, `type`,
  `constructor`, `macro`, `operator`, `keyword`, …
- `span` — byte offsets and line/col, so the client places extmarks without
  re-lexing
- `children` — child node ids, so the client can walk structure

The tree is persistent: on edit, the server re-elaborates only affected
definitions and returns the updated node list. Unchanged subtrees keep their
ids, so the client can diff old vs new and only re-apply what moved.

### Incremental strategy

The expensive part is the prelude (`std/`). It is elaborated once at startup
and cached. Each buffer is elaborated against that cache.

Within a buffer, the incremental granularity is **definitions**, not bytes.
Fun's enforester and elaborator are interleaved — a change at the top of a file
can change the elaboration of everything below. A definition is re-elaborated
only if its text or its dependencies' hashes changed. This is the
[content-addressed codebase](content-addressed-codebase.md)'s
`(definition, context)` cache-key problem. Solving it for highlighting solves
it for the codebase database too.

### Query engine

Highlighting rules are S-expression patterns over the tree, in tree-sitter's
query syntax:

```scheme
(identifier) @variable
(type_identifier) @type
(constructor) @constructor
(operator) @operator
(keyword) @keyword
(macro_use) @macro
(string) @string
(comment) @comment
```

The query engine lives on the **server**, not the client. The client sends a
query (or uses the default), the server runs it, and returns `(span, capture)`
pairs. The client applies them. This keeps the client lean and editor-agnostic
— no client needs to implement a query matcher.

Default queries are bundled with the server. A client can override them by
sending its own query string with `set_query`.

### Highlight groups

The server returns semantic **roles**, not editor-specific groups. Each client
maps roles to its own highlight system:

| role | neovim | VS Code | Emacs |
|---|---|---|---|
| `variable` | `@variable` | `variable.other.qll` | `font-lock-variable-name-face` |
| `type` | `@type` | `entity.name.type.qll` | `font-lock-type-face` |
| `constructor` | `@constructor` | `entity.name.function.qll` | `font-lock-function-name-face` |
| `operator` | `@operator` | `keyword.operator.qll` | `font-lock-builtin-face` |
| `keyword` | `@keyword` | `keyword.control.qll` | `font-lock-keyword-face` |
| `macro` | `@macro` | `entity.name.function.macro.qll` | `font-lock-preprocessor-face` |
| `string` | `@string` | `string.quoted.double.qll` | `font-lock-string-face` |
| `comment` | `@comment` | `comment.line.qll` | `font-lock-comment-face` |

The mapping is a simple dictionary in each client. The server never learns
what editor it is talking to.

## Protocol

msgpack-RPC over stdio. The server reads msgpack arrays from stdin, writes
responses to stdout. MessagePack-CSharp handles (de)serialization; the RPC
layer is a ~150-line loop reading `[type, msgid, method, params]` tuples.

**Why msgpack-RPC:** neovim's job stdio channels speak msgpack-rpc natively,
so `vim.rpcrequest(channel, 'highlight', ...)` works with zero Lua-side
encoding. No other protocol gives that. LSP is the only real alternative
(best multi-editor support) but is JSON and heavy — revisit if VS Code parity
becomes required. Hand-rolled msgpack would re-implement msgpack-RPC worse.

### Multi-session model

A single server process serves multiple editor sessions. Each message
carries a session ID; the server maintains a session dictionary. The
**prelude cache is shared** across all sessions — elaborated once, reused
by everyone. That is the expensive part, and the whole point of a single
server.

```
initialize()                          → { session_id = "abc" }
```

Elaborates the prelude, caches the context, returns a session ID. Called
once per editor session.

```
did_open(session_id, uri, text)       → { tree = { nodes = {...} } }
```

Parses and elaborates a new buffer. Returns the full tree.

```
did_change(session_id, uri, text)     → { tree = { nodes = {...} } }
```

Re-elaborates a changed buffer. The client sends full text (debounced); the
server re-elaborates only affected definitions and returns the updated tree.

```
did_close(session_id, uri)             → { ok = true }
```

Drops the buffer's tree and its cache entry.

```
highlight(session_id, uri, query?)    → { spans = { { span = {...}, capture = "variable" }, ... } }
```

Runs a query over the buffer's tree and returns highlight spans. If `query` is
omitted, the default query is used.

```
set_query(session_id, uri, query)     → { ok = true }
```

Overrides the highlighting query for a buffer. The query is an S-expression
string in tree-sitter's query syntax.

### Error handling

The server never crashes on a parse or elaboration error. It returns:

```
{ error = { message = "...", span = {...} } }
```

The client displays the error and highlights what it can. A buffer with errors
still gets partial highlighting — the tree is best-effort, not all-or-nothing.

## Client design

The client is thin and contains no language-specific logic. Its entire job:

1. Spawn the server (`jobstart` in neovim, `child_process` in VS Code).
2. On buffer open: `did_open(uri, text)`.
3. On buffer change (debounced): `did_change(uri, text)`.
4. On `highlight` response: apply extmarks from the returned spans.
5. Map roles to highlight groups (a dictionary, ~10 entries).

### Neovim client (Lua, ~50 lines)

Uses neovim 0.12 extmark APIs — the modern highlighting path. Extmarks track
text edits and can be individually managed, unlike `nvim_buf_add_highlight`.

```lua
local M = {}

local ns = vim.api.nvim_create_namespace("quill")

local function spawn()
  local channel = vim.fn.jobstart({ "quill", "highlight", "--server" }, { rpc = true })
  local session = vim.fn.rpcrequest(channel, "initialize").session_id
  return channel, session
end

local function apply_highlights(bufnr, spans)
  vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)
  for _, s in ipairs(spans) do
    local group = groups[s.capture] or "@variable"
    vim.api.nvim_buf_set_extmark(bufnr, ns, s.span.start_line, s.span.start_col, {
      end_line = s.span.end_line,
      end_col = s.span.end_col,
      hl_group = group,
    })
  end
end

-- did_open / did_change → rpcrequest → apply_highlights
```

The Lua code is: spawn, send, receive, apply. No parsing, no query matching,
no understanding of quill's grammar.

### Other editors

Neovim-only for now. The server is editor-agnostic (msgpack-RPC), so a VS Code
or Emacs client is a thin shim later — but no multi-editor abstraction is built
now. The protocol is the same; only the client changes.

## Project structure

A new project `Quill.Highlight` (or a `--server` mode on `Quill.Cli`):

```
src/Quill.Highlight/
  Server.cs         -- msgpack-RPC server, stdio transport, session manager
  Tree.cs           -- elaborated tree, node format, incremental updates
  Query.cs          -- S-expression query parser and matcher
  Highlight.cs      -- role → span extraction, default queries
```

The server references `Quill.Compiler` (for the reader, enforester, elaborator)
and `MessagePack-CSharp` (for serialization). It does not reference any editor
library.

## What is hard

**Definition-level incremental enforestation/elaboration.** The codebase-db's
`(definition, context)` cache-key problem. A change at the top of a file can
change the elaboration of everything below. The cache key is the definition's
content hash plus the hash of its elaborated context. This is the real work,
and it is the same work regardless of whether the consumer is a highlighter or
a database.

**Threading span info through the elaborator.** The elaborator already produces
spans, but they are not consistently attached to every identifier. The tree
builder needs to thread spans from the reader through the enforester to the
elaborator and out to the tree.

**The query engine.** A minimal S-expression matcher is ~200 lines of C#. A
full tree-sitter-compatible query engine (with predicates, quantifiers, field
matching) is more. Start minimal; extend when a query needs it.

## What is easy

**The protocol.** msgpack-RPC over stdio is ~150 lines with MessagePack-CSharp
(the RPC loop, not just the serializer). No maintained .NET msgpack-RPC server
library exists, but the loop is trivial: read a 4-tuple, dispatch, write the
response.

**The client.** ~50 lines of Lua, no language-specific logic.

**The tree format.** The compiler already produces `Syntax` trees; the tree
builder attaches roles and flattens to a node list.

**Default queries.** The keyword set is fixed (`TokenTree.cs`); the roles come
from the elaborator. A default query is ~15 S-expression lines.

## Open questions

- **Tree access vs span-only.** Should the client be able to query the tree
  directly (for folding, navigation, go-to-definition), or should the server
  always return spans? The lazy path is span-only; tree access is the upgrade.
- **Multiple buffers.** Does the server elaborate buffers independently, or
  does it track cross-buffer dependencies? Independent is simpler; cross-buffer
  is needed if a project has multiple files that import each other.
- **Query language scope.** How much of tree-sitter's query language to implement?
  Node type matching and captures are enough for highlighting. Fields,
  predicates, and quantifiers are for later consumers.
- **Server lifecycle.** Does the server exit when the last buffer closes, or
  does it idle? Idle is faster for the next buffer; exit is cleaner.

## Relationship to other directions

- [First-class compiler API](first-class-elaborator-api.md) — this server is
  the first consumer that forces the API to be honest about exposing the
  binding table and tree.
- [Content-addressed codebase](content-addressed-codebase.md) — the
  `(definition, context)` cache-key problem is shared. Solving it for
  highlighting solves it for the database.
- [Syntax-highlighting ticket](../tickets/syntax-highlighting.md) — this
  design sharpens the ticket from fog to a decided direction. The remaining
  decision is whether to build it now or after the first-class API lands.
