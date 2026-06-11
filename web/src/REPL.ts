import { Component, createRef } from "preact";
import { html, log } from "@/html.ts";
import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import { examples } from "examples";
import stub from "stub";

function getTheme() {
  if (typeof globalThis === "undefined") return {};
  const getVar = (name: string) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getVar("--bg"),
    foreground: getVar("--fg"),
    cursor: getVar("--cursor"),
    selectionBackground: getVar("--selection"),
  };
}

declare global {
  var impli: REPL;
}

export class REPL extends Component {
  private containerRef = createRef<HTMLDivElement>();
  private xterm: Terminal | null = null;
  private fitter: FitAddon | null = null;
  private echo: LocalEchoAddon | null = null;
  // deno-lint-ignore no-explicit-any
  public exports: any;

  override async componentDidMount() {
    if (!this.containerRef.current) return;

    if (document.readyState !== "complete") {
      await new Promise((resolve) => {
        globalThis.addEventListener("load", resolve, { once: true });
      });
    }

    await document.fonts.ready;

    this.xterm = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getTheme(),
    });

    this.fitter = new FitAddon();
    this.xterm.loadAddon(this.fitter);

    this.echo = new LocalEchoAddon();
    this.xterm.loadAddon(this.echo);

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
      if (index == 0 && (tokens[0] ?? "").startsWith(":")) return metas;
      return [];
    });

    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const files = Object.keys(examples).map((key) =>
        key.startsWith("/") ? key.slice(1) : key
      );
      if (index == 1 && [":load", ":l"].includes(tokens[0])) return files;
      return [];
    });

    this.xterm.options.linkHandler = {
      activate: (_: MouseEvent, uri: string) => {
        globalThis.open(uri, "_blank", "noopener,noreferrer");
      },
      hover: () => {},
      leave: () => {},
      allowNonHttpProtocols: false,
    };

    this.xterm.open(this.containerRef.current);
    this.fitter.fit();
    this.xterm.focus();

    globalThis.addEventListener("resize", () => this.fitter?.fit());

    await this.start();
  }

  public applyTheme() {
    if (this.xterm) {
      this.xterm.options.theme = getTheme();
    }
  }

  private write(text: string) {
    this.echo?.print(text);
  }

  private clear() {
    this.write("\x1bc");
  }

  public async readInput(prompt: string) {
    try {
      return await this.echo!.read(prompt);
    } catch (_) {
      // Ctrl-D — return EOT character to signal EOF
      return "\x04";
    }
  }

  public writeTrace(path: string, trace: string) {
    const file = new File([trace], path, { type: "text/plain" });
    const url = URL.createObjectURL(file);
    globalThis.open(url, "_blank");
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  private async start() {
    log("REPL", "Starting application...");
    globalThis.impli = this;

    const wasi = new WASI({
      args: ["impli"],
      env: {},
      // deno-lint-ignore no-explicit-any
      fs: examples as any,
      stdin: (_) => {
        console.error("WASI stdin requested - this should NEVER EVER happen!");
        this.write("\n");
        this.write("*** ERROR: something has gone horribly wrong...");
        return null;
      },
      stdout: (text) => {
        this.write(text);
      },
      stderr: (text) => {
        this.write(text);
      },
    });

    const exports = {};

    try {
      const wasm = await WebAssembly.instantiateStreaming(
        fetch("./impli.wasm"),
        {
          ...wasi.getImportObject(),
          ghc_wasm_jsffi: stub(exports),
        },
      );

      Object.assign(exports, wasm.instance.exports);

      wasi.initialize(wasm, {
        ghc_wasm_jsffi: stub(exports),
        // deno-lint-ignore no-explicit-any
      } as any);

      this.exports = exports;
      this.exports.start();
      log("REPL", "WASM module loaded and started");
    } catch (error) {
      console.error("Failed to load WASM module:", error);
    }
  }

  override render() {
    return html`
      <div id="terminal" ref="${this.containerRef}" />
    `;
  }
}
