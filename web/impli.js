const wasm = "./impli.wasm";
const glue = "./glue.js";
const __exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch(wasm), {
  ghc_wasm_jsffi: (await import(glue)).default(__exports), // CHECK: why await here? default export is not async
});

// CHECK: should this refer to instance?
Object.assign(__exports, wasm_instance.exports);

// CHECK: what does this do?
// wasi.initialize(wasm_instance);
//
// CHECK: is this correct?
// await __exports.initialize()
