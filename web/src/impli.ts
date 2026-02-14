import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { CAPACITY, dedent, IDLE, log } from "@/util.ts";

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
  public terminal: Terminal;
  private fitter: FitAddon;
  private echo: LocalEchoAddon;
  private lock: Int32Array = new Int32Array(new SharedArrayBuffer(4));
  private buffer: SharedArrayBuffer = new SharedArrayBuffer(CAPACITY);
  private worker: Worker = new Worker(new URL("./worker.ts", import.meta.url), {
    type: "module",
  });

  constructor(container: HTMLElement) {
    this.terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getTheme(),
    });

    this.fitter = new FitAddon();
    this.terminal.loadAddon(this.fitter);

    this.echo = new LocalEchoAddon();
    this.terminal.loadAddon(this.echo);

    this.terminal.open(container);
    this.fitter.fit();
    this.terminal.focus();

    this.setupEventListeners();
  }

  private setupEventListeners() {
    globalThis.addEventListener("resize", () => this.fitter.fit());

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
   * Write IMP trace to new browser tab
   */
  public writeTrace(trace: string) {
    // Create blob from trace
    const blob = new Blob([trace], { type: "text/plain" });

    // Create URL for blob
    const url = URL.createObjectURL(blob);

    // Open blob in new tab
    globalThis.open(url, "_blank");

    // Revoke URL after delay to free memory
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  public async start() {
    log("Impli", "Starting application...");
    this.writeWelcome();

    try {
      // Service Worker for cross-origin isolation
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
            const line = await this.echo.read(prompt);
            const encoded = new TextEncoder().encode(line + "\n");
            const view = new Uint8Array(this.buffer);
            view.fill(0);
            view.set(encoded);

            Atomics.store(this.lock, 0, IDLE);
            Atomics.notify(this.lock, 0, 1);
            log("Impli", "Input sent to worker");
            break;
          }
        }
      };

      this.worker.onerror = (error) => {
        log("Impli", `Worker error: ${error.message}`);
      };

      // Initialize worker
      log("Impli", "Posting initialization message to worker...");
      this.worker.postMessage({
        lock: this.lock,
        buffer: this.buffer,
        wasmURL: "./static/impli.wasm",
      });
    } catch (err) {
      console.error("Initialization failed:", err);
    }
  }

  public dispose() {
    this.worker?.terminate();
    this.terminal.dispose();
  }
}
