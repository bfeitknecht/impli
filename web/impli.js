// TODO

const wasm_url = "./impli.wasm";
const glue_url = "./glue.js";
const __exports = {};

const { instance } = await WebAssembly.instantiateStreaming(fetch(wasm_url), {
  ghc_wasm_jsffi: (await import(glue_url)).default(__exports),
  // CHECK: what needs to be done here?
  // wasi_snapshot_preview1: ...
});

Object.assign(__exports, wasm_instance.exports);

// CHECK: how to export `instance`?
