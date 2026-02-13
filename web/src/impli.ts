import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { DATA_BUFFER_SIZE, STATE_IDLE, log, dedent } from "./util.ts";

const logo = dedent`\x1b[1m\
  ,_  ,_     ,___  ,_    ,_
  | | | |\\/| | |_) | |   | |
  |_| |_|  | |_|   |_|__ |_|
  \x1b[0m`;

const message = dedent`\
  Execute IMP in the browser and inspect resulting state.
  Made with <3 by Basil Feitknecht
  `;

/**
 * Get xterm theme from CSS variables
 */
function getTheme() {
  if (typeof globalThis === "undefined") return {};
  const getVar = (name: string) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getVar("--terminal-bg") || undefined,
    foreground: getVar("--terminal-fg") || undefined,
    cursor: getVar("--terminal-cursor") || undefined,
    selectionBackground: getVar("--terminal-selection") || undefined,
  };
}

export class Impli {
  private terminal: Terminal;
  private fitAddon: FitAddon;
  private localEcho: LocalEchoAddon;
  private worker: Worker | null = null;
  private lock: Int32Array | null = null;
  private buffer: SharedArrayBuffer | null = null;

  constructor(container: HTMLElement) {
    this.terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getTheme(),
    });

    this.fitAddon = new FitAddon();
    this.terminal.loadAddon(this.fitAddon);

    this.localEcho = new LocalEchoAddon();
    this.terminal.loadAddon(this.localEcho);

    this.terminal.open(container);
    this.fitAddon.fit();
    this.terminal.focus();

    this.setupEventListeners();
  }

  private setupEventListeners() {
    globalThis.addEventListener("resize", () => this.fitAddon.fit());

    const applyTheme = () => {
      this.terminal.options.theme = getTheme();
    };

    const mql = globalThis.matchMedia("(prefers-color-scheme: dark)");
    mql.addEventListener("change", applyTheme);

    const observer = new MutationObserver(applyTheme);
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["style", "class"],
    });
  }

  public writeWelcome() {
    this.terminal.write("\x1bc" + logo + "\r\n" + message + "\r\n\r\n");
  }

  public write(text: string) {
    this.terminal.write(text.replace(/\n/g, "\r\n"));
  }

  /**
   * Called by JSFFI stub
   */
  public writeIMP(text: string) {
    this.write(text);
  }

  public async start() {
    log("Impli", "Starting application...");
    this.writeWelcome();

    try {
      // 1. Service Worker for cross-origin isolation
      log("Impli", "Registering service worker...");
      await navigator.serviceWorker.register("./sw.ts", {
        scope: "./",
        type: "module",
      });
      await navigator.serviceWorker.ready;

      if (!navigator.serviceWorker.controller) {
        await new Promise((r) => setTimeout(r, 100));
        globalThis.location.reload();
        return;
      }

      // 2. Shared buffers for sync communication
      log("Impli", "Creating shared buffers...");
      this.lock = new Int32Array(new SharedArrayBuffer(4));
      this.buffer = new SharedArrayBuffer(DATA_BUFFER_SIZE);

      // 3. Start backend worker
      log("Impli", "Starting worker...");
      this.worker = new Worker(new URL("./worker.ts", import.meta.url), {
        type: "module",
      });

      this.worker.onmessage = async (event) => {
        const { type, text, prompt } = event.data;
        switch (type) {
          case "stdout":
          case "stderr":
            this.write(text);
            break;
          case "stdin": {
            if (!this.lock || !this.buffer) return;
            log("Impli", "Stdin requested:", prompt);
            const line = await this.localEcho.read(prompt);
            const encoded = new TextEncoder().encode(line + "\n");
            const bufferView = new Uint8Array(this.buffer);
            bufferView.fill(0);
            bufferView.set(encoded);

            Atomics.store(this.lock, 0, STATE_IDLE);
            Atomics.notify(this.lock, 0, 1);
            log("Impli", "Input sent to worker");
            break;
          }
        }
      };

      this.worker.onerror = (error) => {
        this.write(`\r\n\x1b[31mWorker error: ${error.message}\x1b[0m\r\n`);
      };

      // 4. Initialize worker
      log("Impli", "Posting initialization message to worker...");
      this.worker.postMessage({
        lock: this.lock,
        buffer: this.buffer,
        wasmURL: "./impli.wasm",
      });
    } catch (err) {
      console.error("Initialization failed:", err);
      this.write(`\r\n\x1b[31mInitialization failed: ${err}\x1b[0m\r\n`);
    }
  }

  public dispose() {
    this.worker?.terminate();
    this.terminal.dispose();
  }
}
