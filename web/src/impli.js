import { WASI } from "npm:@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const __exports = {};
export async function create(source) {
  const wasi = new WASI([], [], []);
  const instance = await WebAssembly.instantiateStreaming(fetch(source), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(__exports),
  });
  Object.assign(__exports, instance.exports);
  wasi.initialize(instance); // ERROR: instance.exports_initialize() is undefined
  return instance;
}
