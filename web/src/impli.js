import { WASI } from "npm:@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

export class IMPLI {
  constructor() {}

  async initialize() {
    this.exports = {};
    const wasi = new WASI([], [], []);
    const wasm = await WebAssembly.instantiateStreaming(fetch("./impli.wasm"), {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: ghc_wasm_jsffi(this.exports),
    });
    wasi.initialize(wasm.instance);
    Object.assign(this.exports, wasm.instance.exports);

    this.pointer = this.exports.initialize();
    return this;
  }

  async interpret(input) {
    await this.exports.execute(this.pointer, input);
  }
}
