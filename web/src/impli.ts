import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import { examples } from "examples";
import stub from "stub";
import { dedent, log } from "@/util.ts";

const logo = dedent`\x1b[1m
  o  _ _   _   ) o
  ( ) ) ) )_) (  (
         (
  \x1b[0m`;

const repository = "https://github.com/bfeitknecht/impli";

const banner = dedent`\
  Execute IMP in the browser and inspect resulting state.
  Visit the \x1b]8;;${repository}\x1b\\repository\x1b]8;;\x1b\\ and leave a star!
  If you're into formal methods, check out the \x1b]8;;${repository}/blob/master/docs/paper/IMP.pdf\x1b\\whitepaper\x1b]8;;\x1b\\ too.
  Made with <3 by Basil Feitknecht.
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

    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const metas = [
        ":help",
        ":quit",
        ":clear",
        ":version",
        ":set",
        ":unset",
        ":reset",
        ":show",
        ":load",
        ":write",
        ":ast",
      ];
      if (index == (0) && (tokens[0] ?? "").startsWith(":")) {
        return metas;
      }
      return [];
    });
    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const files = Object.keys(examples).map((key) =>
        key.startsWith("/") ? key.slice(1) : key
      );
      if (index == 1 && [":load", ":l"].includes(tokens[0])) {
        return files;
      }
      return [];
    });

    const activateLink = (_: MouseEvent, uri: string) => {
      globalThis.open(uri, "_blank", "noopener,noreferrer");
    };

    const linkHandler = {
      activate: activateLink,
      hover: () => {},
      leave: () => {},
      allowNonHttpProtocols: false,
    };

    this.terminal.options.linkHandler = linkHandler;

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
    const message = "\x1bc" +
      logo +
      "\n" +
      banner +
      "\n\n";
    this.write(message);
  }

  public writeTips() {
    const message = dedent`\
      Here's a list of things to get you up to speed.
        - 'print' followed by an expression outputs its evaluation
        - 'read' followed by some variable name assigns the input to it
        - 'x += 1' increments the variable named 'x'
          - This principle also works to decrement, multiply, divide, and take the modulo
        - ':load prime.imp' interprets the named file
          - In this case that exposes the procedure 'prime'
          - Invoke it and display the result with e.g. 'prime(31; p); print p'
      ` +
      "\n\n";
    this.write(message);
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
    this.writeTips();

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
