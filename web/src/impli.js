import { WASI, ConsoleStdout } from "npm:@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import { Terminal } from "./term.js";

class IMPLI {
  constructor() {
    this.terminal = new Terminal();
    this.exports = {};
  }

  async init() {
    const fds = [
      [], // stdin
      ConsoleStdout.lineBuffered(this.terminal.write), // stdout
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

async () => await new IMPLI().init();
