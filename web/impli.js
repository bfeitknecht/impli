import { Terminal } from "xterm";
import { openpty } from "xterm-pty";
import { FitAddon } from "xterm-fit";
import { WASI } from "runno";
import stub from "stub";

const logo = dedent`\
  ,_  ,_     ,___  ,_    ,_
  | | | |\\/| | |_) | |   | |
  |_| |_|  | |_|   |_|__ |_|
  `;

const message = dedent`\
  Execute IMP in the browser and inspect resulting state.
  Made with <3 by Basil Feitknecht`;

export class Impli {
  constructor() {
    // Terminal instance
    const terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getThemeStyle(),
    });
    terminal.open(document.getElementById("terminal"));
    this.terminal = terminal;
  }

  async start() {
    // PTY pair
    const { master, slave } = openpty();
    this.terminal.loadAddon(master);

    // Write welcome message
    slave.write("\x1bc\x1b[1m" + logo + "\x1b[0m" + message); // CHECK: Escape codes supported?

    // FitAddon
    const fitter = new FitAddon();

    // WASI instance
    const wasi = new WASI({
      stdin: () => {},
      stdout: () => {},
      stderr: () => {},
    });

    // Placeholder exports
    const exports = {};

    // WASM instance
    const wasm = await WebAssembly.instantiateStreaming(fetch("./impli.wasm"), {
      ...wasi.getImportObject(),
      ghc_wasm_jsffi: stub(exports),
    });

    // Knot tying
    Object.assign(exports, wasm.instance.exports);

    // Initialize and setup
    wasi.initialize(wasm, { ghc_wasm_jsffi: stub(exports) });
    wasi.instance.exports.setup();
  }
}

// Get xterm theme from CSS variables
function getThemeStyle() {
  const getStyleVar = (name) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getStyleVar("--terminal-bg") || undefined,
    foreground: getStyleVar("--terminal-fg") || undefined,
    cursor: getStyleVar("--terminal-cursor") || undefined,
    selectionBackground: getStyleVar("--terminal-selection") || undefined,
  };
}
