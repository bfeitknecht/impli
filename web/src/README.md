## `impli` Web — Frontend Source

| File                         | Description                                                                      |
| ---------------------------- | -------------------------------------------------------------------------------- |
| [`main.ts`](main.ts)         | Page: ghostty-web terminal, REPL loop, Ctrl-C, downloads for `:write`            |
| [`worker.ts`](worker.ts)     | Web worker hosting `impli.wasm`, bridging its JSFFI imports to the page          |
| [`readline.ts`](readline.ts) | Line editor: history, completion, multi-line continuation, paste line by line    |
| [`protocol.ts`](protocol.ts) | Messages between page and worker                                                 |

The interpreter session lives in the WASM instance inside the worker, so a
reload starts fresh. The page submits lines, the worker streams output back
and asks for `read` input. Long-running interpretation yields to the worker's
event loop every few thousand steps, where a Ctrl-C from the page is noticed
and interrupts it without losing the session.
