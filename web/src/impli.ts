import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import { examples } from "examples";
import stub from "stub";
import { dedent, log } from "@/util.ts";

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

declare global {
  var impli: Impli;
}

export class Impli {
  public terminal: Terminal;
  private fitter: FitAddon;
  private echo: LocalEchoAddon;
  public exports: any;

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

  public write(text: string) {
    this.terminal.write(text.replace(/\n/g, "\r\n"));
  }

  public writeWelcome() {
    this.terminal.write("\x1bc" + logo + "\r\n" + message + "\r\n\r\n");
  }

  public async readInput(prompt: string) {
    const line = await this.echo.read(prompt);
    return line;
  }

  public writeTrace(path: string, trace: string) {
    const file = new File([trace], path, { type: "text/plain" });
    const url = URL.createObjectURL(file);
    globalThis.open(url, "_blank");
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  public async start() {
    log("Impli", "Starting application...");
    globalThis.impli = this;
    this.writeWelcome();

    const wasi = new WASI({
      args: ["impli"],
      env: {},
      fs: examples,
      stdin: (_) => {
        console.error("WASI stdin requested (this should never happen)");
        return null;
      },
      stdout: (text) => {
        this.write(text);
      },
      stderr: (text) => {
        this.write(text);
      },
    });

    // Placeholder exports
    const exports = {};

    // Instantiate WASM
    try {
      const wasm = await WebAssembly.instantiateStreaming(
        fetch("./impli.wasm"),
        {
          ...wasi.getImportObject(),
          ghc_wasm_jsffi: stub(exports),
        },
      );

      // Knot-tying, fill exports with actual instance exports
      Object.assign(exports, wasm.instance.exports);

      // Initialize WASI
      wasi.initialize(wasm, {
        ghc_wasm_jsffi: stub(exports),
      });

      // Expose exports
      this.exports = exports;

      // Start WASM
      this.exports.start();
      log("Impli", "WASM module loaded and started");
    } catch (error) {
      console.error("Failed to load WASM module:", error);
    }
  }
}
