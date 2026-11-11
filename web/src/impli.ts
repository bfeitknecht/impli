import { FitAddon, Terminal, init as initGhostty } from "ghostty-web";
import type { ITheme } from "ghostty-web";
import { LocalEchoAddon } from "@gytx/xterm-local-echo";
import { WASI } from "@runno/wasi";
import { examples } from "examples";
import stub from "stub";
import { dedent, log } from "@/util.ts";
import {
  Envelope,
  showFault,
  StateJson,
  StepsJson,
  StmNode,
  stmToSource,
  TraceEntryJson,
} from "@/protocol.ts";

const logo = dedent`\x1b[1m
  o  _ _   _   ) o
  ( ) ) ) )_) (  (
         (
  \x1b[0m`;

const repository = "https://github.com/bfeitknecht/impli";
const paper = "https://bfeitknecht.github.io/impli/IMP.pdf";
const banner = dedent`\0\
  The IMP Language Interpreter in the browser!
  Visit the \x1b]8;;${repository}\x1b\\repository\x1b]8;;\x1b\\ and check out the \x1b]8;;${paper}\x1b\\paper\x1b]8;;\x1b\\.
  Made with <3 by Basil Feitknecht.
  `;

/** Version of the WASM exports record, as declared in `impli.cabal`. */
export interface ImpliExports {
  start: () => void;
  evaluate: (line: string) => string;
  resolveInput: (value: string) => string;
  resolveChoice: (side: string) => string;
  resolveHavoc: (value: string) => string;
  interrupt: () => void;
  getPrompt: () => string;
  helpText: () => string;
  versionText: () => string;
  showState: () => string;
  showTrace: () => string;
  showSteps: () => string;
  parseAst: (source: string) => string;
  resetSession: (aspect: string) => string;
}

/** Current interpreter state, refreshed after each completed drive. */
export interface SessionView {
  state: StateJson;
  trace: TraceEntryJson[];
  steps: StepsJson;
}

/** Initialize the libghostty WASM backend; must resolve before constructing `Terminal`. */
export async function initTerminal(): Promise<void> {
  await initGhostty();
}

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
  public exports!: ImpliExports;
  private running: boolean;
  /** Whether the driver is awaiting a suspension resolution read. */
  private resolving: boolean;
  /** Whether the last aborted read was aborted by Ctrl-D. */
  private eofSeen: boolean;
  public onSession: ((view: SessionView) => void) | null = null;
  public onAst: ((node: StmNode) => void) | null = null;

  constructor(container: HTMLElement) {
    this.terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getTheme(),
    });

    this.fitter = new FitAddon();
    this.terminal.loadAddon(this.fitter);

    this.echo = new LocalEchoAddon({ enableIncompleteInput: true });
    this.terminal.loadAddon(this.echo);
    this.running = false;
    this.resolving = false;
    this.eofSeen = false;
    this.echo.onEof(() => {
      this.eofSeen = true;
    });

    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const metas = [
        ":help",
        ":tips",
        ":quit",
        ":clear",
        ":version",
        ":reset",
        ":show",
        ":load",
        ":write",
        ":ast",
      ];
      if (index == 0 && (tokens[0] ?? "").startsWith(":")) {
        return metas;
      }
      return [];
    });
    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const aspects = ["all", "vars", "procs", "break", "trace"];
      if (index == 1 && [":reset", ":show"].includes(tokens[0] ?? "")) {
        return aspects;
      }
      return [];
    });
    this.echo.addAutocompleteHandler((index: number, tokens: Array<string>) => {
      const files = Object.keys(examples).map((key) =>
        key.startsWith("/") ? key.slice(1) : key
      );
      if (index == 1 && [":load", ":write"].includes(tokens[0])) {
        return files;
      }
      return [];
    });

    // INFO: ghostty-web detects OSC-8 hyperlinks and URLs natively and opens
    // them on Ctrl/Cmd+click, no link handler is required.

    // Ctrl-C cancels a pending suspension resolution; otherwise the
    // local-echo addon clears the input line, shell behaviour.
    // ghostty-web swallows the event on `true`, the inverse of xterm.js.
    this.terminal.attachCustomKeyEventHandler((event: KeyboardEvent) => {
      const keyC = event.key.toLowerCase() === "c";
      if (event.type === "keydown" && event.ctrlKey && keyC && this.resolving) {
        this.handleInterrupt();
        return true;
      }
      return false;
    });

    this.terminal.open(container);
    this.fitter.fit();
    this.terminal.focus();

    this.setupEventListeners();
  }

  private setupEventListeners() {
    globalThis.addEventListener("resize", () => this.fitter.fit());

    const applyTheme = () => {
      const theme = getTheme();
      this.terminal.options.theme = theme;
      // INFO: runtime theme changes are not yet applied by ghostty-web
      // itself, so poke the internal canvas renderer as well.
      const internals = this.terminal as unknown as {
        renderer?: { setTheme?: (theme: ITheme) => void };
      };
      internals.renderer?.setTheme?.(theme);
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

  public refit() {
    this.fitter.fit();
    this.terminal.focus();
  }

  public writeWelcome() {
    const message = "\x1bc" + dedent`\
      ${logo}
      ${banner}
      \n`;
    this.write(message);
  }

  public writeTips() {
    const message = dedent`\
      Here are some tips to get up to speed. Tab-autocomplete works for meta-commands and filenames.
          - 'print' followed by an arithmetic expression outputs its evaluation
          - 'read' followed by a variable name assigns the integer input to it
          - 'x := 1' defines, 'x += 1' increments the named variable
              - This principle also works to decrement, multiply, divide, and take the modulo
          - ':load prime.imp' interprets the named file
              - In this case that defines the procedure 'prime'
              - To compute and print the k-th prime number enter 'prime(k; p); print p'
      \n`;
    this.write(message);
  }

  private handleInterrupt() {
    if (!this.exports) return;
    this.exports.interrupt();
    this.echo.abortRead("^C");
  }

  /** Read one line with the given prompt, null on Ctrl-C or Ctrl-D abort. */
  private async readLine(prompt: string): Promise<string | null> {
    this.eofSeen = false;
    try {
      return await this.echo.read(prompt, "+ ");
    } catch (_) {
      return null;
    } finally {
      // neutralize this.resolving in the caller
    }
  }

  /** Query the WASM side for the current session view and notify listeners. */
  public refreshSession() {
    if (!this.exports || !this.onSession) return;
    try {
      const state = JSON.parse(this.exports.showState()) as StateJson;
      const trace = JSON.parse(this.exports.showTrace()) as TraceEntryJson[];
      const steps = JSON.parse(this.exports.showSteps()) as StepsJson;
      this.onSession({ state, trace, steps });
    } catch (error) {
      log("Impli", "Failed to refresh session view:", error);
    }
  }

  /** Open the traced statement of the n-th newest statement in the AST tab. */
  public openAst(node: StmNode) {
    if (this.onAst) this.onAst(node);
  }

  public async start() {
    log("Impli", "Starting application...");
    this.writeWelcome();

    const wasi = new WASI({
      args: ["impli"],
      env: {},
      fs: examples as never, // Build script constructs a WASI-compatible FS
      stdin: (_) => {
        console.error("WASI stdin requested - this should NEVER EVER happen!");
        this.write("\n");
        this.write("*** ERROR: something has gone horribly wrong...");
        return null;
      },
    });

    // Placeholder imports for knot-tying, filled with the instance exports.
    const exports: Partial<ImpliExports> = {};

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
      this.exports = exports as ImpliExports;

      // Initialize WASI
      wasi.initialize(wasm, {
        ghc_wasm_jsffi: stub(exports),
      } as never);

      log("Impli", "WASM module loaded and started");
      this.refreshSession();
      this.running = true;
      // escape is only possible via :quit or Ctrl-D
      while (this.running) {
        const line = await this.readLine(this.exports.getPrompt());
        if (line === null) {
          await this.quit(); // Ctrl-D at the prompt, exit cleanly
          break;
        }
        await this.drive(line);
      }
    } catch (error) {
      console.error("Failed to load WASM module:", error);
      this.write("\n*** ERROR: failed to load the interpreter");
    }
  }

  /** Drive one line: either a metacommand handled here or IMP source in WASM. */
  public async drive(line: string) {
    const trimmed = line.trim();
    if (trimmed === ":)") {
      this.terminal.writeln("You look good today!"); // it's true
      return;
    }
    if (trimmed.startsWith(":")) {
      await this.metacommand(trimmed.slice(1));
      return;
    }
    if (trimmed === "") return;
    let envelope = JSON.parse(this.exports.evaluate(line)) as Envelope;
    while (true) {
      for (const event of envelope.events) this.terminal.writeln(event);
      const status = envelope.status;
      switch (status.tag) {
        case "done":
          this.refreshSession();
          return;
        case "error":
          for (const line of showFault(status.fault).split("\n")) {
            this.terminal.writeln(line);
          }
          return;
        case "await-input":
        case "await-havoc": {
          const text = await this.resolveInteractiveInput(
            status.tag,
            status.variable,
            status.retry,
          );
          if (text === null) return; // aborted by Ctrl-C or Ctrl-D
          envelope = JSON.parse(
            status.tag === "await-input"
              ? this.exports.resolveInput(text)
              : this.exports.resolveHavoc(text),
          ) as Envelope;
          continue;
        }
        case "await-choice": {
          const side = await this.resolveInteractiveChoice(status);
          if (side === null) return; // aborted by Ctrl-C or Ctrl-D
          envelope = JSON.parse(this.exports.resolveChoice(side)) as Envelope;
          continue;
        }
      }
    }
  }

  /** Read an input or havoc resolution value, null on abort. */
  private async resolveInteractiveInput(
    tag: string,
    variable: string,
    retry: boolean,
  ): Promise<string | null> {
    this.resolving = true;
    const text = await this.readLine(this.promptFor(tag, variable, retry));
    this.resolving = false;
    if (text === null) {
      this.exports.interrupt();
      this.terminal.writeln("*** ERROR: interrupted");
      return null;
    }
    return text.trim() === ""
      ? String(Math.floor(Math.random() * 2147483647) - 1073741823)
      : text.trim();
  }

  private promptFor(tag: string, variable: string, retry: boolean): string {
    const prefix = retry ? "invalid input, again: " : "";
    return tag === "await-havoc"
      ? `${prefix}${variable} := [random] `
      : `${prefix}${variable} := `;
  }

  /** Interactive non-deterministic or parallel choice screen, null on abort. */
  private async resolveInteractiveChoice(
    status: { focus?: string; stm?: StmNode },
  ): Promise<string | null> {
    const node = status.stm;
    let left = "first";
    let right = "second";
    if (node && (node.tag === "nondet" || node.tag === "par")) {
      left = stmToSource(node.left as StmNode);
      right = stmToSource(node.right as StmNode);
    } else if (status.focus) {
      left = status.focus;
    }
    this.terminal.writeln("\x1b[1mchoose:\x1b[0m");
    this.terminal.writeln(`  1) ${left}`);
    this.terminal.writeln(`  2) ${right}`);
    this.resolving = true;
    const text = await this.readLine("choose 1 or 2: ");
    this.resolving = false;
    if (text === null) {
      this.exports.interrupt();
      this.terminal.writeln("*** ERROR: interrupted");
      return null;
    }
    return text.trim() === "1" ? "true" : "false";
  }

  /** Handle a metacommand (without the leading colon) on the JavaScript side. */
  private async metacommand(meta: string) {
    const [name, ...rest] = meta.trim().split(/\s+/);
    const args = rest.join(" ");
    switch (abbreviation(name)) {
      case "help":
        this.terminal.writeln(this.exports.helpText().trimEnd());
        break;
      case "tips":
        this.writeTips();
        break;
      case "quit":
        await this.quit();
        break;
      case "clear":
        this.terminal.clear();
        break;
      case "version":
        this.terminal.writeln(this.exports.versionText());
        break;
      case "reset":
        this.metaReset(args);
        break;
      case "show":
        this.metaShow(args);
        break;
      case "load":
        await this.metaLoad(args);
        break;
      case "write":
        this.metaWrite(args);
        break;
      case "ast":
        this.metaAst(args);
        break;
      default:
        this.terminal.writeln(`unrecognized meta command: :${meta}`);
        this.terminal.writeln(
          "Enter :help to list available metacommands or :quit to exit.",
        );
    }
  }

  private metaReset(aspect: string) {
    const result = JSON.parse(this.exports.resetSession(aspect.trim()));
    if (result.tag === "ok") {
      this.terminal.writeln(`+++ INFO: ${aspect.trim() || "environment"} reset`);
      this.refreshSession();
    } else {
      this.terminal.writeln(`*** ERROR: ${result.message}`);
    }
  }

  private metaShow(aspect: string) {
    const view: StateJson = JSON.parse(this.exports.showState());
    const which = aspect.trim() || "all";
    const printVars = () => {
      const entries = Object.entries(view.vars);
      if (entries.length === 0) this.terminal.writeln("(empty)");
      for (const [name, value] of entries) {
        this.terminal.writeln(`${name} := ${value}`);
      }
    };
    const printProcs = () => {
      const entries = Object.entries(view.procs);
      if (entries.length === 0) this.terminal.writeln("(empty)");
      for (const [name, proc] of entries) {
        this.terminal.writeln(
          `procedure ${name}(${proc.params.join(",")};${proc.returns.join(",")}) begin ... end`,
        );
      }
    };
    const printState = () => {
      printVars();
      printProcs();
      this.terminal.writeln(`break: ${view.break}`);
    };
    switch (which) {
      case "all":
        printState();
        return;
      case "vars":
        printVars();
        return;
      case "procs":
        printProcs();
        return;
      case "break":
        this.terminal.writeln(`break: ${view.break}`);
        return;
      case "trace": {
        const trace = JSON.parse(this.exports.showTrace()) as TraceEntryJson[];
        if (trace.length === 0) this.terminal.writeln("(empty)");
        for (const entry of trace) {
          this.terminal.writeln(`${entry.index}: ${entry.source}`);
        }
        return;
      }
      default:
        this.terminal.writeln(`*** ERROR: unknown aspect: ${which}`);
    }
  }

  private async metaLoad(file: string) {
    const path = file.trim().startsWith("/") ? file.trim() : `/${file.trim()}`;
    const records = examples as unknown as Record<string, { content: string }>;
    const entry = records[path];
    if (!entry) {
      this.terminal.writeln(`*** ERROR: file not found: ${file}`);
      return;
    }
    await this.drive(entry.content);
    this.terminal.writeln(`+++ INFO: interpreted: ${path.slice(1)}`);
  }

  private metaWrite(path: string) {
    const name = path.trim() || "trace.imp";
    const trace = JSON.parse(this.exports.showTrace()) as TraceEntryJson[];
    const source = trace
      .slice()
      .sort((a, b) => b.index - a.index)
      .map((entry) => entry.source)
      .join("\n");
    const blob = new Blob([source], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = name;
    anchor.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    this.terminal.writeln(`+++ INFO: wrote trace to: ${name}`);
  }

  /** Parse IMP source (or `#n` for the n-th traced statement) into the AST tab. */
  public parseAstInput(input: string) {
    this.metaAst(input);
  }

  private metaAst(input: string) {
    if (input.trim().startsWith("#")) {
      const n = parseInt(input.trim().slice(1), 10);
      const trace = JSON.parse(this.exports.showTrace()) as TraceEntryJson[];
      const entry = trace.find((e) => e.index === n);
      if (!entry) {
        this.terminal.writeln(`*** ERROR: index out of bounds: #${n}`);
        return;
      }
      this.openAst(entry.ast);
      return;
    }
    const result = JSON.parse(this.exports.parseAst(input));
    if (result.tag === "ok") {
      const construct = result.construct;
      let node: StmNode;
      switch (construct.tag) {
        case "statement":
          node = construct.statement;
          break;
        case "arithmetic":
          node = construct.expr;
          break;
        case "boolean":
          node = construct.expr;
          break;
        default:
          this.terminal.writeln("+++ INFO: whitespace");
          return;
      }
      this.openAst(node);
    } else {
      for (const line of String(result.message).split("\n")) {
        this.terminal.writeln(line);
      }
    }
  }

  /** Clear the session, say goodbye and disable the terminal. */
  private async quit() {
    this.exports.resetSession("");
    this.terminal.writeln("Goodbye!");
    this.running = false;
    this.terminal.options.disableStdin = true;
  }
}

/** Expand first-letter abbreviations of metacommands to their full names. */
function abbreviation(name: string): string {
  const metas: { [name: string]: string } = {
    h: "help",
    t: "tips",
    q: "quit",
    c: "clear",
    v: "version",
    r: "reset",
    s: "show",
    l: "load",
    w: "write",
    a: "ast",
  };
  return metas[name] ?? name;
}


