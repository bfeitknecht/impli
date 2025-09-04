import { WASI, File, OpenFile, ConsoleStdout } from "npm:@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

export class IMPLI {
  constructor() {}

  async initialize() {
    this.exports = {};
    const fds = [
      new OpenFile(new File([])), // stdin
      ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)), // stdout
      ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)), // stderr
    ];
    const wasi = new WASI([], [], fds);
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
    const response = await this.exports.execute(this.pointer, input);
    const result = JSON.parse(response);
    return result;
  }
}
