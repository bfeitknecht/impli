import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import stub from "./stub.js";

/**
 * Get xterm theme from CSS variables
 */
function getTheme() {
  const getVar = (name) => getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getVar("--terminal-bg") || undefined,
    foreground: getVar("--terminal-fg") || undefined,
    cursor: getVar("--terminal-cursor") || undefined,
    selectionBackground: getVar("--terminal-selection") || undefined,
  };
}

/**
 * Dedent template literal helper
 */
function dedent(strings, ...values) {
  let raw = "";
  for (let i = 0; i < strings.length; i++) {
    raw += strings[i];
    if (i < values.length) raw += values[i];
  }
  const lines = raw.split("\n");
  const indent = lines
    .filter((line) => line.trim())
    .reduce((min, line) => {
      const match = line.match(/^(\s*)/);
      return match ? Math.min(min, match[1].length) : min;
    }, Infinity);

  if (indent === Infinity) return raw;

  return lines
    .map((line) => line.slice(indent))
    .join("\n")
    .replace(/\n/g, "\r\n");
}

const logo = dedent`\
  ,_  ,_     ,___  ,_    ,_
  | | | |\\/| | |_) | |   | |
  |_| |_|  | |_|   |_|__ |_|
  `;

const message = dedent`\
  Execute IMP in the browser and inspect resulting state.
  Made with <3 by Basil Feitknecht
  `;

/**
 * Impli REPL class
 */
export class Impli {
  constructor() {
    // Create Terminal instance
    const terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getTheme(),
    });

    // Open terminal in DOM
    terminal.open(document.getElementById("terminal"));

    // Create and register FitAddon
    const fitAddon = new FitAddon();
    terminal.loadAddon(fitAddon);
    fitAddon.fit();
    globalThis.addEventListener("resize", () => fitAddon.fit());

    // Setup dynamic theme handling
    const applyTheme = () => {
      terminal.options.theme = getTheme();
    };

    // Re-apply on system theme changes
    const mql = globalThis.matchMedia("(prefers-color-scheme: dark)");
    if ("addEventListener" in mql) {
      mql.addEventListener("change", applyTheme);
    } else if ("addListener" in mql) {
      mql.addListener(applyTheme);
    }

    // Observe :root style changes for manual CSS variable overrides
    const observer = new MutationObserver(applyTheme);
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["style", "class"],
    });

    // Create local-echo addon for line editing and history
    this.localEcho = new LocalEchoAddon();
    terminal.loadAddon(this.localEcho);

    // Enter terminal
    terminal.focus();
    this.terminal = terminal;
  }

  /**
   * Write welcome message to terminal
   */
  writeWelcome() {
    // Clear screen and write bold logo
    this.terminal.write("\x1bc\x1b[1m" + logo + "\x1b[0m\n" + message + "\n\n");
  }

  /**
   * Read from the terminal (called by JSFFI)
   */
  async readInput() {
    // Use local-echo to read a line with full editing support
    const prompt = await this.exports.getPrompt();
    const line = await this.localEcho.read(prompt);
    return line + "\n";
  }

  /**
   * Start the impli REPL
   */
  async start() {
    // Immediately globally expose instance before WASM starts
    globalThis.impli = this;
    // console.log("[DEBUG] Impli instance exposed globally");

    // Write welcome message
    this.writeWelcome();

    // Create WASI instance with pty slave for stdin/stdout/stderr
    const wasi = new WASI({
      args: ["impli"],
      env: {},
      stdin: () => {
        // Not used - input comes through JSFFI
        console.log("[WARN] WASI stdin called (should not happen)");
      },
      stdout: (data) => {
        const text = typeof data === "string" ? data : new TextDecoder().decode(data);
        // Write directly to terminal, normalize line endings
        this.terminal.write(text.replace(/\n/g, "\r\n"));
      },
      stderr: (data) => {
        const text = typeof data === "string" ? data : new TextDecoder().decode(data);
        // Write directly to terminal, normalize line endings
        this.terminal.write(text.replace(/\n/g, "\r\n"));
      },
    });

    // Placeholder exports
    const exports = {};

    // Instantiate WASM
    // console.log("[DEBUG] Starting WASM instantiation...");
    try {
      // Fetch and instantiate WASM module
      const wasm = await WebAssembly.instantiateStreaming(fetch("./impli.wasm"), {
        ...wasi.getImportObject(),
        ghc_wasm_jsffi: stub(exports),
      });

      // Knot-tying, fill exports with actual instance exports
      Object.assign(exports, wasm.instance.exports);

      // Initialize WASI
      wasi.initialize(wasm, {
        ghc_wasm_jsffi: stub(exports),
      });

      // Expose exports
      this.exports = exports;

      // Start WASM
      // console.log("[DEBUG] Calling wasi.instance.exports.start()...");
      wasi.instance.exports.start();
      console.log("[INFO] WASM module loaded and started");
    } catch (error) {
      console.error("[ERROR] Failed to load WASM module:", error);
      this.slave.write("\r\n\x1b[31mError: Failed to load WASM module\x1b[0m\r\n");
      this.slave.write(`${error.message}\r\n`);
    }
  }

  /**
   * Write trace to new browser tab
   */
  writeTrace(trace) {
    // Create blob from trace
    const blob = new Blob([trace], { type: "text/plain" });

    // Create URL for blob
    const url = URL.createObjectURL(blob);

    // Open blob in new tab
    globalThis.open(url, "_blank");

    // Revoke URL after delay to free memory
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
}

// Expose Impli class globally
globalThis.Impli = Impli;
