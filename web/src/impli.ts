import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import { examples } from "examples";
import { dedent, log } from "@/util.ts";

const repository = "https://github.com/bfeitknecht/impli";
const paper = "https://bfeitknecht.github.io/impli/IMP.pdf";
const tips = dedent`\
  Here are some tips to get up to speed. Tab-autocomplete works for meta-commands and filenames.
      - 'print' followed by an arithmetic expression outputs its evaluation
      - 'read' followed by a variable name assigns the integer input to it
      - 'x := 1' defines, 'x += 1' increments the named variable
          - This principle also works to decrement, multiply, divide, and take the modulo
      - ':load prime.imp' interprets the named file
          - In this case that defines the procedure 'prime'
          - To compute and print the k-th prime number enter 'prime(k; p); print p'
  \n`;

function getTheme() {
  if (typeof globalThis === "undefined") return {};
  const getVar = (name: string) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getVar("--terminal-bg"),
    foreground: getVar("--terminal-fg"),
    cursor: getVar("--terminal-cursor"),
    selectionBackground: getVar("--terminal-selection"),
  };
}

export class Impli {
  public terminal: Terminal;
  private fitter: FitAddon;
  private echo: LocalEchoAddon;
  private inputQueue: number[] = [];
  private readingInput = false;
  private encoder = new TextEncoder();

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
        ":tips",
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
      if (index === 0 && (tokens[0] ?? "").startsWith(":")) {
        return metas;
      }
      return [];
    });
    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const files = Object.keys(examples).map((key) =>
        key.startsWith("/") ? key.slice(1) : key
      );
      const command = tokens[0];
      if (index === 1 && typeof command === "string" &&
        [":load", ":l"].includes(command)) {
        return files;
      }
      return [];
    });

    const activateLink = (_: MouseEvent, uri: string) => {
      globalThis.open(uri, "_blank", "noopener,noreferrer");
    };

    this.terminal.options.linkHandler = {
      activate: activateLink,
      hover: () => {},
      leave: () => {},
      allowNonHttpProtocols: false,
    };

    this.terminal.attachCustomKeyEventHandler((event) => {
      if (event.type === "keydown" && event.ctrlKey && !event.altKey &&
        !event.metaKey && !event.shiftKey && event.key.toLowerCase() === "c") {
        this.enqueueLine(":interrupt");
        this.write("^C\r\n");
        return false;
      }
      return true;
    });

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
    this.echo.print(text);
  }

  private enqueueBytes(data: Uint8Array) {
    for (const byte of data) this.inputQueue.push(byte);
  }

  private enqueueLine(line: string) {
    this.enqueueBytes(this.encoder.encode(line + "\n"));
  }

  private async requestLineInput() {
    if (this.readingInput) return;
    this.readingInput = true;
    try {
      const line = await this.echo.read("");
      if ([":tips", ":t"].includes(line)) {
        this.write(tips);
        this.enqueueLine("");
      } else {
        this.enqueueLine(line);
      }
    } catch {
      // Ctrl-D / EOF
      this.enqueueBytes(Uint8Array.of(4));
    } finally {
      this.readingInput = false;
    }
  }

  private nextStdinByte() {
    if (this.inputQueue.length > 0) return this.inputQueue.shift();
    this.requestLineInput();
    return null;
  }

  private writeChunk(chunk: unknown) {
    if (typeof chunk === "number") {
      this.write(String.fromCharCode(chunk));
      return;
    }
    if (chunk instanceof Uint8Array) {
      this.write(new TextDecoder().decode(chunk));
      return;
    }
    if (typeof chunk === "string") {
      this.write(chunk);
      return;
    }
    this.write(String(chunk));
  }

  public async start() {
    log("Impli", "Starting application...");
    this.write(
      dedent`\
        The IMP Language Interpreter in the browser!
        Visit the \x1b]8;;${repository}\x1b\\repository\x1b]8;;\x1b\\ and check out the \x1b]8;;${paper}\x1b\\paper\x1b]8;;\x1b\\.
        Runtime mode: WASI stdio message stream.
        \n`,
    );

    const wasi = new WASI({
      args: ["impli"],
      env: {},
      fs: examples as any,
      stdin: () => this.nextStdinByte(),
      stdout: (chunk: unknown) => this.writeChunk(chunk),
      stderr: (chunk: unknown) => this.writeChunk(chunk),
    });

    try {
      const wasm = await WebAssembly.instantiateStreaming(
        fetch("./impli.wasm"),
        wasi.getImportObject() as any,
      );

      if (typeof (wasi as any).start === "function") {
        (wasi as any).start(wasm as any);
      } else if (typeof (wasi as any).initialize === "function") {
        (wasi as any).initialize(wasm as any, {});
      }

      const exports = (wasm as any).instance?.exports ?? (wasm as any).exports;
      if ((wasi as any).started !== true && typeof exports?.start === "function") {
        exports.start();
      }

      log("Impli", "WASM module loaded and started");
    } catch (error) {
      console.error("Failed to load WASM module:", error);
      this.write(`\n*** ERROR: failed to load WASM module: ${String(error)}\n`);
    }
  }
}
