# Web Rework Brainstorm

This note captures implementation approaches for a larger web overhaul while preserving the interpreter as a WASM component.

## Constraints from issue discussion

- Keep the core interpreter logic in WASM.
- Keep the browser target as an HTM + Preact application.
- Keep xterm.js for terminal rendering.
- Separate terminal, inference tree, and header into distinct frontend components.
- Add support for automatic inference-tree construction and EBNF syntax checks.
- Make the WASM interpreter usable outside the browser as well (for example via Wasmtime) with REPL-like behavior.

## Candidate architecture options

### Option A: Keep current JSFFI-coupled runtime

- Continue exposing browser-specific callbacks from JS into Haskell/WASM.
- Extend the existing app with more UI features directly on top.

Pros:
- Lowest migration effort.

Cons:
- Keeps tight coupling between runtime and browser integration details.
- Hard to reuse in Wasmtime/native host context.
- Does not address current xterm.js workaround pain points cleanly.

### Option B: Message-passing interpreter runtime (recommended)

- Define a small host/runtime protocol where the host sends `(statement, current state)` (or command + state) messages and receives next-state / output events.
- Implement a single WASM-facing runtime loop for the interpreter and expose protocol entrypoints.
- Build host adapters:
  - Browser adapter (Preact app, xterm.js, inference-tree panel, syntax panel)
  - Wasmtime adapter (native-style REPL loop)

Pros:
- Preserves one interpreter core while supporting browser and native-like hosts.
- Eliminates need for browser-global `getLine` workarounds by making input explicit messages.
- Enables deterministic testing and replay of interpreter steps/events.

Cons:
- Medium migration effort (protocol design and adapter implementation).

### Option C: Fully split browser/native implementations

- Keep one parser/semantics library but maintain separate host-specific execution stacks.

Pros:
- Straightforward host-specific optimization.

Cons:
- Higher long-term maintenance burden and feature drift risk.

## Recommended decomposition (frontend)

- `Header` component: logo, mode toggles, docs links.
- `TerminalPane` component: xterm.js rendering and command input.
- `InferenceTreePane` component: derived rule trees (rendered HTML).
- `SyntaxPane` component: EBNF checker diagnostics.
- Shared state manager (signals/store) that consumes runtime protocol events.

## Runtime protocol sketch

A minimal protocol that supports both browser and Wasmtime hosts:

### Request

```json
{ "id": 1, "kind": "Eval", "input": "x := 1", "state": { ... } }
```

### Response

```json
{
  "id": 1,
  "ok": true,
  "nextState": { ... },
  "events": [
    { "type": "stdout", "text": "..." },
    { "type": "inferenceTree", "html": "..." },
    { "type": "syntax", "errors": [] }
  ]
}
```

Notes:
- `events` decouple UI rendering concerns from interpreter evaluation.
- The same message model can be wrapped in browser `postMessage` or direct Wasmtime host calls.

## Inference-tree generation approach

- Keep rule source of truth in Typst definitions.
- Add an offline build step that compiles Typst rules to embeddable HTML fragments (or JSON + templates).
- Emit tree events from the runtime with rule IDs + substitutions.
- Let `InferenceTreePane` map runtime events to precompiled rule HTML.

## EBNF syntax checker approach

- Reuse existing parser/lexer from interpreter where possible.
- Expose a `CheckSyntax` protocol message that returns structured parse diagnostics.
- Surface diagnostics in `SyntaxPane` and optionally inline in terminal output.

## Phased migration plan

1. Define protocol types and add a minimal request/response runtime entrypoint.
2. Implement a Wasmtime host adapter proving native-like REPL parity.
3. Refactor browser app into `Header`, `TerminalPane`, `InferenceTreePane`, `SyntaxPane`.
4. Replace `getLine` workaround with protocol-driven explicit input events.
5. Add inference-tree and syntax-check features incrementally on top of protocol events.

This path keeps the existing strengths (WASM interpreter + Preact frontend) while making the runtime host-agnostic and easier to extend.
