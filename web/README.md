## `impli` Web

The `impli` REPL in the browser: a [ghostty-web](https://github.com/coder/ghostty-web)
terminal talking to the Haskell interpreter compiled to WebAssembly, running
client-side in a web worker. Built with [Deno](https://deno.com) and deployed
to [GitHub Pages](https://bfeitknecht.github.io/impli). See
[`src/README.md`](src/README.md) for the architecture.

### Build

From the repository root, build the WASM binary with Nix, copy it and its
JSFFI stub into `web/public`, and bundle the frontend:

```sh
nix run .#build
```

For faster iteration on the Haskell side, use the dev shell's cross compiler:

```sh
nix develop -c bash -c '
  wasm32-wasi-cabal build exe:impli-web --builddir=dist-wasm
  cp $(wasm32-wasi-cabal list-bin exe:impli-web --builddir=dist-wasm) web/public/impli.wasm
  $(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i web/public/impli.wasm -o web/public/stub.js
  cd web && deno task build'
```

`build.ts` generates `examples.js` from `docs/examples`, mirrors `static/`
and bundles `src/main.ts` and `src/worker.ts`. Everything ends up in the
ignored `public/` directory.

### Test and Serve

```sh
cd web
deno task smoke   # headless test of the WASM build: examples, input, big integers, interrupts
deno task serve   # serve public/ locally
```

### Deployment

The GitHub Actions workflow (`.github/workflows/deploy.yaml`) runs
`nix run .#build` on every push to `master` and deploys `web/public/` to
GitHub Pages.
