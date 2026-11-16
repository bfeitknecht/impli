/**
 * Web REPL of the IMP language interpreter: a full-viewport ghostty-web
 * terminal talking to the interpreter worker. The page owns line editing,
 * the worker owns the session.
 */
import { FitAddon, init, Terminal } from "ghostty-web";
import type { ITheme } from "ghostty-web";
import { examples } from "examples";
import { Readline } from "@/readline.ts";
import { adjust, creation, label, link } from "@/render.ts";
import type { Request, Response } from "@/protocol.ts";

const CONTINUATION = "IMP| ";

const REPOSITORY = "https://github.com/bfeitknecht/impli";
const PAPER = "https://bfeitknecht.github.io/impli/IMP.pdf";


const BANNER = [
  "\x1b[1m",
  "o  _ _   _   ) o",
  "( ) ) ) )_) (  (",
  "       (",
  "\x1b[0m",
  "The IMP Language Interpreter in the browser!",
  `Visit the ${label("repository")} and check out the ${label("paper")}.`,
  "Made with <3 by Basil Feitknecht.",
  "",
  "",
].join("\n");

const TIPS = [
  "Here are some tips to get up to speed. Tab-autocomplete works for metacommands and filenames.",
  "    - 'print' followed by an arithmetic expression outputs its evaluation",
  "    - 'read' followed by a variable name assigns the integer input to it",
  "    - 'x := 1' defines, 'x += 1' increments the named variable",
  "        - This principle also works to decrement, multiply, divide, and take the modulo",
  "    - ':load prime.imp' interprets the named file",
  "        - In this case that defines the procedure 'prime'",
  "        - To compute and print the k-th prime number enter 'prime(k; p); print p'",
  "",
  "",
].join("\n");

/** CSS custom property of the palette in styles.css. */
const color = (name: string) => getComputedStyle(document.documentElement).getPropertyValue(`--${name}`).trim();

/** Terminal theme from the palette in styles.css. */
function theme(): ITheme {
  const ansi = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"];
  const palette: Record<string, string> = {};
  for (const name of ansi) {
    palette[name] = color(`ansi-${name}`);
    palette[`bright${name[0].toUpperCase()}${name.slice(1)}`] = color(`ansi-bright-${name}`);
  }
  return {
    background: color("bg"),
    foreground: color("fg"),
    cursor: color("fg"),
    // INFO: Selection inverts, like the hovered links
    selectionBackground: color("fg"),
    selectionForeground: color("bg"),
    ...palette,
  };
}

/** Offer a file to download, for `:write`. */
function download(path: string, content: string) {
  const link = document.createElement("a");
  link.href = URL.createObjectURL(new Blob([content], { type: "text/plain" }));
  link.download = path.split("/").pop() || "trace.imp";
  link.click();
  setTimeout(() => URL.revokeObjectURL(link.href), 1000);
}

async function main() {
  await init();
  const container = document.getElementById("terminal")!;
  const initial = creation(theme());
  const terminal = new Terminal({
    cursorBlink: true,
    fontFamily: '"CommitMono", ui-monospace, Menlo, monospace',
    fontSize: 14,
    theme: initial,
  });
  const fitter = new FitAddon();
  terminal.loadAddon(fitter);
  terminal.open(container);
  fitter.fit();
  new ResizeObserver(() => fitter.fit()).observe(container);
  terminal.focus();

  const retheme = adjust(terminal, initial);
  link(terminal, container, { repository: REPOSITORY, paper: PAPER });
  globalThis.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => retheme(theme()));

  const readline = new Readline(terminal);
  readline.print(BANNER);
  const worker = new Worker("./worker.mjs", { type: "module" });
  const send = (message: Request) => worker.postMessage(message);

  // Requests answered by the worker, matched by id
  let counter = 0;
  const pending = new Map<number, (value: unknown) => void>();
  const ask = <T>(make: (id: number) => Request) =>
    new Promise<T>((resolve) => {
      const id = counter++;
      pending.set(id, resolve as (value: unknown) => void);
      send(make(id));
    });

  const incomplete = (input: string) => ask<boolean>((id) => ({ type: "incomplete", id, input }));
  const never = () => Promise.resolve(false);

  const files = Object.keys(examples).map((name) => name + ".imp");
  readline.completer = async (word, before) => {
    const head = before[0] ?? "";
    if (before.length === 1 && [":load", ":l"].includes(head)) return Promise.resolve(files.filter((f) => f.startsWith(word)));
    if (before.length === 1 && [":show", ":s", ":reset", ":r"].includes(head)) {
      return Promise.resolve(["vars", "procs", "trace"].filter((a) => a.startsWith(word)));
    }
    if (before.length === 1 && head === ":set") return Promise.resolve(["semantics"].filter((a) => a.startsWith(word)));
    if (before.length === 2 && head === ":set") {
      return Promise.resolve(["natural", "structural"].filter((a) => a.startsWith(word)));
    }
    const candidates = await ask<string[]>((id) => ({ type: "complete", id, prefix: word }));
    return ":tips".startsWith(word) && word.startsWith(":") ? [...candidates, ":tips"].sort() : candidates;
  };

  // Ctrl-C outside of reads interrupts interpretation
  let running = false;
  terminal.attachCustomKeyEventHandler((event: KeyboardEvent) => {
    if (event.type === "keydown" && event.ctrlKey && event.key.toLowerCase() === "c" && running && !readline.reading) {
      send({ type: "cancel" });
      return true; // INFO: ghostty-web swallows the event on true
    }
    return false;
  });

  // Prompt reflects the verbosity level, updated after every submission
  let prompt = "IMP> ";
  let finished: (() => void) | null = null;
  let started: (() => void) | null = null;
  const ready = new Promise<void>((resolve) => (started = resolve));

  worker.onmessage = async (event: MessageEvent<Response>) => {
    const message = event.data;
    switch (message.type) {
      case "ready":
        prompt = message.prompt;
        started?.();
        return;
      case "output":
        readline.print(message.text + "\n");
        return;
      case "clear":
        // INFO: The banner survives clearing
        terminal.write("\x1b[2J\x1b[3J\x1b[H");
        readline.print(BANNER);
        return;
      case "save":
        download(message.path, message.content);
        return;
      case "ask": {
        const result = await readline.read(message.prompt, message.prompt, never);
        send(
          result.tag === "line"
            ? { type: "answer", value: result.line }
            : result.tag === "eof"
            ? { type: "answer", value: "\x04" }
            : { type: "cancel" },
        );
        return;
      }
      case "done":
        prompt = message.prompt;
        finished?.();
        return;
      case "reply":
        pending.get(message.id)?.(message.value);
        pending.delete(message.id);
        return;
      case "failed":
        readline.print(`*** ERROR: failed to start interpreter: ${message.message}\n`);
        return;
    }
  };

  await ready;
  while (true) {
    const result = await readline.read(prompt, CONTINUATION, incomplete);
    if (result.tag !== "line") continue; // INFO: Ctrl-C and Ctrl-D give a fresh prompt
    if ([":tips", ":t"].includes(result.line.trim())) {
      readline.print(TIPS);
      continue;
    }
    running = true;
    const done = new Promise<void>((resolve) => (finished = resolve));
    send({ type: "submit", line: result.line });
    await done;
    running = false;
    readline.flush();
  }
}

main();
