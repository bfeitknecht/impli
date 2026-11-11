import { FitAddon, Terminal, init as initGhostty } from "ghostty-web";
import type { ITheme } from "ghostty-web";
import { WASI } from "@runno/wasi";
import { Readline, ReadResult } from "@/readline.ts";
import { examples } from "examples";
import stub from "stub";
import { dedent, log } from "@/util.ts";
import {
  DerivationJson,
  deriveProfile,
  Envelope,
  FlightJson,
  SessionJson,
  showFault,
  StateJson,
  Status,
  StepJson,
  TraceEntryJson,
  traceViewOf,
  visibleState,
  StmNode,
  stmToSource,
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

/**
 * Step budget per interpretation chunk: each WASM call performs at most
 * this many structural steps before yielding back to the event loop,
 * mirroring `defaultFuel` of `src/Frontend/Web/Main.hs`.
 */
const FUEL = "16384";

/** localStorage key under which the session persists across page loads. */
const SESSION_KEY = "impli:session:v2";

/** Version of the WASM exports record, as declared in `impli.cabal`. */
export interface ImpliExports {
  start: () => void | Promise<void>;
  bootSession: () => string | Promise<string>;
  evaluate: (
    session: string,
    line: string,
    fuel: string,
  ) => string | Promise<string>;
  advance: (session: string, fuel: string) => string | Promise<string>;
  resolveInput: (
    session: string,
    value: string,
    fuel: string,
  ) => string | Promise<string>;
  resolveChoice: (
    session: string,
    side: string,
    fuel: string,
  ) => string | Promise<string>;
  resolveHavoc: (
    session: string,
    value: string,
    fuel: string,
  ) => string | Promise<string>;
  cancel: (session: string) => string | Promise<string>;
  getPrompt: () => string | Promise<string>;
  helpText: () => string | Promise<string>;
  versionText: () => string | Promise<string>;
  parseAst: (source: string) => string | Promise<string>;
  deriveTree: (
    session: string,
    index: string,
  ) => string | Promise<string>;
  resetSession: (
    session: string,
    aspect: string,
  ) => string | Promise<string>;
}

/** Current interpreter state, refreshed after each completed drive or step. */
export interface SessionView {
  state: StateJson;
  trace: TraceEntryJson[];
  steps: { steps: StepJson[]; profile: ReturnType<typeof deriveProfile> };
  /** In-flight interpretation, exposed live for the stepping tab. */
  flight: FlightJson | null;
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

/** Yield one event-loop turn so key events can fire between WASM chunks. */
function yieldEventLoop(): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, 0));
}

export class Impli {
  public terminal: Terminal;
  private fitter: FitAddon;
  private readline: Readline;
  public exports!: ImpliExports;
  private running: boolean;
  /** Whether a line read is currently active (the reader owns Ctrl-C then). */
  private reading: boolean;
  /**
   * The interpreter session, owned here as plain JSON: every WASM API call
   * takes it as an argument and every envelope carries the update back.
   */
  private session: SessionJson | null;
  /** Steps accumulated across the chunks of the current interpretation. */
  private steps: StepJson[];
  /** Set by Ctrl-C between chunk boundaries; stops the interpretation loop. */
  private ctrlCRequested: boolean;
  /** Set via the stepping tab to stop a run-all loop. */
  private stepStopRequested: boolean = false;
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

    this.readline = new Readline(this.terminal);
    this.running = false;
    this.reading = false;
    this.session = null;
    this.steps = [];
    this.ctrlCRequested = false;

    this.readline.addCompletion((index: number, tokens: Array<string>) => {
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
    this.readline.addCompletion((index: number, tokens: Array<string>) => {
      const aspects = ["all", "vars", "procs", "break", "trace"];
      if (index == 1 && [":reset", ":show"].includes(tokens[0] ?? "")) {
        return aspects;
      }
      return [];
    });
    this.readline.addCompletion((index: number, tokens: Array<string>) => {
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

    // Ctrl-C during a line read is delivered to the reader as a byte and
    // aborts the read; outside of reads it only requests interruption, which
    // the interpretation loop honors after the current chunk returns to the
    // event loop — at most one step budget of latency.
    // ghostty-web swallows the event on `true`, the inverse of xterm.js.
    this.terminal.attachCustomKeyEventHandler((event: KeyboardEvent) => {
      const keyC = event.key.toLowerCase() === "c";
      if (event.type === "keydown" && event.ctrlKey && keyC) {
        if (this.reading) return false;
        this.ctrlCRequested = true;
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
    this.readline.print(text);
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

  /** Adopt the session and step delta carried by an envelope. */
  private adopt(envelope: Envelope) {
    this.session = envelope.session;
    this.steps.push(...envelope.steps);
  }

  /** Persist the current session for resumption across page loads. */
  private persist() {
    if (!this.session) return;
    try {
      localStorage.setItem(SESSION_KEY, JSON.stringify(this.session));
    } catch (_) {
      // Non-fatal: private-mode browsers may refuse.
    }
  }

  /** Restore the persisted session, if any. */
  private restore(): SessionJson | null {
    try {
      const raw = localStorage.getItem(SESSION_KEY);
      return raw ? (JSON.parse(raw) as SessionJson) : null;
    } catch (_) {
      return null;
    }
  }

  /** Read one line with the given prompt; continuation prompt is `+ `. */
  private async readLine(prompt: string): Promise<ReadResult> {
    this.reading = true;
    try {
      return await this.readline.read(prompt, "+ ");
    } finally {
      this.reading = false;
    }
  }

  /** Build the current session view and notify listeners. */
  public refreshSession() {
    if (!this.session || !this.onSession) return;
    try {
      this.onSession({
        state: visibleState(this.session.environment),
        trace: traceViewOf(this.session.trace),
        steps: { steps: this.steps, profile: deriveProfile(this.steps) },
        flight: this.session.flight,
      });
    } catch (error) {
      log("Impli", "Failed to refresh session view:", error);
    }
  }

  /** Open the traced statement of the n-th newest statement in the AST tab. */
  public openAst(node: StmNode) {
    if (this.onAst) this.onAst(node);
  }

  /** Cancel the suspended or running interpretation after a Ctrl-C request. */
  private async cancelSession() {
    if (!this.session) return;
    this.session = JSON.parse(
      await this.exports.cancel(JSON.stringify(this.session)),
    ) as SessionJson;
    this.terminal.writeln("*** ERROR: interrupted");
    this.refreshSession();
    this.persist();
    this.ctrlCRequested = false;
  }

  /** Adopt a stepping envelope, print its events, refresh and persist. */
  private finishStepping(envelope: Envelope): Status {
    this.adopt(envelope);
    for (const event of envelope.events) this.terminal.writeln(event);
    this.refreshSession();
    this.persist();
    return envelope.status;
  }

  /** Begin stepping through the given source, without taking any step yet. */
  public async stepBegin(source: string): Promise<Status | null> {
    if (!this.session) return null;
    this.steps = [];
    this.ctrlCRequested = false;
    return this.finishStepping(
      JSON.parse(
        await this.exports.evaluate(
          JSON.stringify(this.session),
          source,
          "0",
        ),
      ) as Envelope,
    );
  }

  /** Advance the in-flight interpretation by exactly one small step. */
  public async stepOnce(): Promise<Status | null> {
    if (!this.session) return null;
    return this.finishStepping(
      JSON.parse(
        await this.exports.advance(JSON.stringify(this.session), "1"),
      ) as Envelope,
    );
  }

  /** Resolve a pending input or havoc suspension from the stepping tab. */
  public async stepResolveInput(
    text: string,
    havoc: boolean,
  ): Promise<Status | null> {
    if (!this.session) return null;
    const raw = JSON.stringify(this.session);
    const response = havoc
      ? await this.exports.resolveHavoc(raw, text, FUEL)
      : await this.exports.resolveInput(raw, text, FUEL);
    return this.finishStepping(JSON.parse(response) as Envelope);
  }

  /** Resolve a pending choice suspension from the stepping tab. */
  public async stepResolveChoice(left: boolean): Promise<Status | null> {
    if (!this.session) return null;
    return this.finishStepping(
      JSON.parse(
        await this.exports.resolveChoice(
          JSON.stringify(this.session),
          left ? "true" : "false",
          FUEL,
        ),
      ) as Envelope,
    );
  }

  /**
   * Continue the in-flight interpretation to the next pause point in chunked
   * runs, honoring stop requests and Ctrl-C; mirrors the terminal drive loop.
   */
  public async stepRun(): Promise<Status> {
    if (!this.session) return { tag: "error", fault: { tag: "no-flight" } };
    this.ctrlCRequested = false;
    let status: Status = { tag: "running" };
    while (status.tag === "running") {
      await yieldEventLoop();
      if (this.ctrlCRequested || this.stepStopRequested) {
        await this.cancelSession();
        this.stepStopRequested = false;
        return { tag: "error", fault: { tag: "interrupted" } };
      }
      status = this.finishStepping(
        JSON.parse(
          await this.exports.advance(JSON.stringify(this.session), FUEL),
        ) as Envelope,
      );
    }
    return status;
  }

  /** Ask an ongoing stepRun loop to stop after the current chunk. */
  public requestStopStepping() {
    this.stepStopRequested = true;
  }

  /** Cancel the flight shown in the stepping tab. */
  public async stepCancel() {
    await this.cancelSession();
  }

  /**
   * Derive the natural-semantics proof tree of a trace entry, exactly
   * replaying the recorded run that produced it.
   */
  public async deriveTree(index: number): Promise<DerivationJson | null> {
    if (!this.session) return null;
    return JSON.parse(
      await this.exports.deriveTree(JSON.stringify(this.session), String(index)),
    ) as DerivationJson;
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

      // Session resumes from localStorage or boots fresh
      this.session = this.restore() ??
        JSON.parse(await this.exports.bootSession());
      this.persist();
      this.refreshSession();

      log("Impli", "WASM module loaded and started");
      this.running = true;
      // escape is only possible via :quit or Ctrl-D
      while (this.running) {
        const result = await this.readLine(await this.exports.getPrompt());
        if (result.tag === "eof") {
          await this.quit(); // Ctrl-D at the prompt, exit cleanly
          break;
        }
        if (result.tag === "interrupt") continue; // Ctrl-C: fresh prompt
        await this.drive(result.line);
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
      this.terminal.writeln("You look good today!");
      return;
    }
    if (trimmed.startsWith(":")) {
      await this.metacommand(trimmed.slice(1));
      return;
    }
    if (trimmed === "" || !this.session) return;

    this.steps = [];
    this.ctrlCRequested = false;
    let envelope = JSON.parse(
      await this.exports.evaluate(JSON.stringify(this.session), line, FUEL),
    ) as Envelope;

    while (true) {
      this.adopt(envelope);
      for (const event of envelope.events) this.terminal.writeln(event);
      const status = envelope.status;
      switch (status.tag) {
        case "done":
          this.refreshSession();
          this.persist();
          return;
        case "error":
          for (const line of showFault(status.fault).split("\n")) {
            this.terminal.writeln(line);
          }
          this.refreshSession();
          this.persist();
          return;
        case "running": {
          // Mid-flight pause on fuel exhaustion: yield to the event loop
          // so Ctrl-C can land, then continue with the next chunk.
          await yieldEventLoop();
          if (this.ctrlCRequested) {
            await this.cancelSession();
            return;
          }
          envelope = JSON.parse(
            await this.exports.advance(JSON.stringify(this.session), FUEL),
          ) as Envelope;
          continue;
        }
        case "await-input":
        case "await-havoc": {
          const text = await this.resolveInteractiveInput(
            status.tag,
            status.variable,
            status.retry,
          );
          if (text === null) return; // cancelled by Ctrl-C or Ctrl-D
          this.ctrlCRequested = false;
          envelope = JSON.parse(
            status.tag === "await-input"
              ? await this.exports.resolveInput(JSON.stringify(this.session), text, FUEL)
              : await this.exports.resolveHavoc(JSON.stringify(this.session), text, FUEL),
          ) as Envelope;
          continue;
        }
        case "await-choice": {
          const side = await this.resolveInteractiveChoice(status);
          if (side === null) return; // cancelled by Ctrl-C or Ctrl-D
          this.ctrlCRequested = false;
          envelope = JSON.parse(
            await this.exports.resolveChoice(JSON.stringify(this.session), side, FUEL),
          ) as Envelope;
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
    const result = await this.readLine(this.promptFor(tag, variable, retry));
    if (result.tag !== "line") {
      await this.cancelSession();
      return null;
    }
    return result.line.trim() === ""
      ? String(Math.floor(Math.random() * 2147483647) - 1073741823)
      : result.line.trim();
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
    const result = await this.readLine("choose 1 or 2: ");
    if (result.tag !== "line") {
      await this.cancelSession();
      return null;
    }
    return result.line.trim() === "1" ? "true" : "false";
  }

  /** Handle a metacommand (without the leading colon) on the JavaScript side. */
  private async metacommand(meta: string) {
    const [name, ...rest] = meta.trim().split(/\s+/);
    const args = rest.join(" ");
    switch (abbreviation(name)) {
      case "help":
        this.terminal.writeln((await this.exports.helpText()).trimEnd());
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
        this.terminal.writeln(await this.exports.versionText());
        break;
      case "reset":
        await this.metaReset(args);
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

  private async metaReset(aspect: string) {
    if (!this.session) return;
    const result = JSON.parse(
      await this.exports.resetSession(JSON.stringify(this.session), aspect.trim()),
    );
    if (result.tag === "ok") {
      this.session = result.session as SessionJson;
      this.terminal.writeln(`+++ INFO: ${aspect.trim() || "environment"} reset`);
      this.refreshSession();
      this.persist();
    } else {
      this.terminal.writeln(`*** ERROR: ${result.message}`);
    }
  }

  private metaShow(aspect: string) {
    if (!this.session) return;
    const view = visibleState(this.session.environment);
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
        const trace = traceViewOf(this.session.trace);
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
    if (!this.session) return;
    const name = path.trim() || "trace.imp";
    const source = this.session.trace
      .slice()
      .reverse()
      .map((row) => stmToSource(row.statement))
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
  public async parseAstInput(input: string) {
    await this.metaAst(input);
  }

  private async metaAst(input: string) {
    if (input.trim().startsWith("#")) {
      const n = parseInt(input.trim().slice(1), 10);
      const trace = traceViewOf(this.session?.trace ?? []);
      const entry = trace.find((e) => e.index === n);
      if (!entry) {
        this.terminal.writeln(`*** ERROR: index out of bounds: #${n}`);
        return;
      }
      this.openAst(entry.ast);
      return;
    }
    const result = JSON.parse(await this.exports.parseAst(input));
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
    if (this.session) {
      const result = JSON.parse(
        await this.exports.resetSession(JSON.stringify(this.session), ""),
      );
      if (result.tag === "ok") {
        this.session = result.session as SessionJson;
        this.persist();
      }
    }
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
