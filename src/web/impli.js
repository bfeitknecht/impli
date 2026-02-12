import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
const DEBUG = true;

function log(...args) {
  if (DEBUG) {
    console.log("[DEBUG] Impli:", ...args);
  }
}

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

// State values for lock
const STATE_IDLE = 0;

const DATA_BUFFER_SIZE = 4096; // 4KB for input data

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

    // Create local-echo addon
    this.localEcho = new LocalEchoAddon();
    terminal.loadAddon(this.localEcho);

    terminal.focus();
    this.terminal = terminal;

    // Worker
    this.worker = null;
  }

  /**
   * Write welcome message
   */
  writeWelcome() {
    this.terminal.write("\x1bc\x1b[1m" + logo + "\x1b[0m\n" + message + "\n\n");
  }

  /**
   * Write to terminal
   */
  write(text) {
    this.terminal.write(text.replace(/\n/g, "\r\n"));
  }

  /**
   * Handle stdin request from worker
   */
  async stdin(lock, buffer, prompt) {
    const line = await this.localEcho.read(prompt);
    log("Input received:", line);

    // Write input to buffer
    const text = line + "\n";
    const encoded = new TextEncoder().encode(text);
    const bufferView = new Uint8Array(buffer);
    bufferView.fill(0);
    bufferView.set(encoded);

    // Signal ready (return to idle) and wake worker
    Atomics.store(lock, 0, STATE_IDLE);
    Atomics.notify(lock, 0, 1);

    log("Input sent to worker");
  }

  /**
   * Start the worker
   */
  async start() {
    log("Starting impli...");

    // Write welcome message
    this.writeWelcome();

    // Create shared buffers
    const lock = new Int32Array(new SharedArrayBuffer(4));
    const buffer = new SharedArrayBuffer(DATA_BUFFER_SIZE);
    log("Shared buffers created");

    // Create worker
    this.worker = new Worker("./worker.js", { type: "module" });
    log("Worker created");

    // Handle worker messages
    this.worker.onmessage = async (event) => {
      const { type, text, prompt } = event.data;

      switch (type) {
        case "stdout":
          this.write(text);
          break;

        case "stderr":
          this.write(text);
          break;

        case "stdin":
          log("Stdin requested by worker");
          await this.stdin(lock, buffer, prompt);
          break;

        default:
          log("Unknown message type:", type);
      }
    };

    this.worker.onerror = (error) => {
      log("Worker error event:", error);
      this.write(`\r\n\x1b[31mWorker error: ${error.message}\x1b[0m\r\n`);
    };

    // Initialize and start worker
    this.worker.postMessage({ lock, buffer, wasmURL: "./impli.wasm" });

    log("Initialization complete");
  }
}

// Export Impli class
export { Impli };
