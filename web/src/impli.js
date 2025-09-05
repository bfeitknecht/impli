import { WASI, ConsoleStdout } from "npm:@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const config = {
  prompt: "IMP> ",
  welcome: "Welcome to the IMP REPL! Enter :help to list available metacommands.",
};

class Terminal extends globalThis.Terminal {
  constructor() {
    super({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New Bold"',
      fontSize: 13,
    });
    this.config = config;
    this.open(document.getElementById("terminal"));
    this.writeln(this.config.welcome);
    this.write(config.prompt);
  }
}

class IMPLI {
  constructor() {
    this.terminal = new Terminal();
    this.exports = {};
  }

  async init() {
    const fds = [
      [], // stdin
      ConsoleStdout.lineBuffered(this.terminal.writeln), // stdout
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

(async () => {
  globalThis.impli = await new IMPLI().init();
})();
