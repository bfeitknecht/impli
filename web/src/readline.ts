/**
 * Line reader for the ghostty-web terminal: line editing with cursor
 * movement, history and tab completion, multi-line continuation decided by
 * an (asynchronous) completeness check, and an explicit Ctrl-C/Ctrl-D result
 * instead of thrown aborts.
 *
 * Terminal input is queued and processed one key or line at a time, so
 * pasted text containing newlines is entered line by line.
 */

/** Structural subset of the terminal the reader relies on. */
export interface ReadlineTerminal {
  cols: number;
  write(data: string): void;
  onData(listener: (data: string) => void): { dispose(): void };
}

/** Outcome of a read: line entered, Ctrl-C interrupt, or Ctrl-D end-of-input. */
export type ReadResult =
  | { tag: "line"; line: string }
  | { tag: "interrupt" }
  | { tag: "eof" };

/** Completion candidates for the word under the cursor, given the words before it. */
export type Completer = (word: string, before: string[]) => Promise<string[]>;

/** Whether accumulated input is incomplete and continues on the next line. */
export type Incomplete = (input: string) => Promise<boolean>;

/** Split terminal input into keys: escape sequences, control bytes and text runs. */
function keys(data: string): string[] {
  const result: string[] = [];
  const pattern = /\x1b\[[0-9;]*[A-Za-z~]|\x1bO[A-Za-z]|\x1b.|[\x00-\x1f\x7f]|[^\x00-\x1f\x7f\x1b]+/g;
  for (const match of data.matchAll(pattern)) result.push(match[0]);
  return result;
}

/** History entry as a single editable line, dropping line comments. */
function flatten(entry: string): string {
  return entry.split("\n").map((line) => line.replace(/\/\/.*$/, "").trim())
    .filter((line) => line.length > 0).join(" ");
}

export class Readline {
  private term: ReadlineTerminal;
  private history: string[] = [];
  private queue: string[] = [];
  private handler: ((key: string) => Promise<void>) | null = null;
  private busy = false;
  public completer: Completer = () => Promise.resolve([]);

  constructor(term: ReadlineTerminal) {
    this.term = term;
    term.onData((data) => {
      // Strip bracketed paste markers, normalize line endings
      const text = data.replace(/\x1b\[20[01]~/g, "").replace(/\r\n?/g, "\r").replace(/\n/g, "\r");
      this.queue.push(...keys(text));
      this.drain();
    });
  }

  /** Whether a read is in progress. */
  public get reading(): boolean {
    return this.handler !== null;
  }

  /** Print text, normalizing newlines to terminal carriage semantics. */
  public print(text: string) {
    this.term.write(text.replace(/\r?\n/g, "\r\n"));
  }

  /** Discard pending input, e.g. keys typed during interpretation. */
  public flush() {
    this.queue = [];
  }

  private async drain() {
    if (this.busy) return;
    this.busy = true;
    try {
      while (this.handler && this.queue.length > 0) {
        await this.handler(this.queue.shift()!);
      }
    } finally {
      this.busy = false;
    }
  }

  /** Read one logical line, continuing while input is incomplete. */
  public read(prompt: string, continuation: string, incomplete: Incomplete): Promise<ReadResult> {
    return new Promise((resolve) => {
      const lines: string[] = [];
      let buffer = "";
      let cursor = 0;
      let current = prompt;
      let historyIndex: number | null = null;
      let saved = "";
      let lastTab = 0;
      let drawn = 0; // INFO: Row of the terminal cursor relative to the prompt row

      const finish = (result: ReadResult) => {
        this.handler = null;
        resolve(result);
      };

      const position = (k: number): [number, number] => {
        const cols = Math.max(1, this.term.cols);
        if (k === 0) return [0, 0];
        if (k % cols === 0) return [Math.floor((k - 1) / cols), cols - 1]; // pending wrap
        return [Math.floor(k / cols), k % cols];
      };

      const render = () => {
        let out = drawn > 0 ? `\x1b[${drawn}A` : "";
        out += "\r\x1b[0J" + current + buffer;
        const [endRow] = position(current.length + buffer.length);
        const [row, col] = position(current.length + cursor);
        if (endRow > row) out += `\x1b[${endRow - row}A`;
        out += "\r" + (col > 0 ? `\x1b[${col}C` : "");
        drawn = row;
        this.term.write(out);
      };

      const below = () => {
        const [endRow] = position(current.length + buffer.length);
        const down = endRow - drawn;
        drawn = 0;
        return (down > 0 ? `\x1b[${down}B` : "") + "\r\n";
      };

      const insert = (text: string) => {
        buffer = buffer.slice(0, cursor) + text + buffer.slice(cursor);
        cursor += text.length;
      };

      const complete = async () => {
        const head = buffer.slice(0, cursor);
        const word = head.match(/[^\s();,]*$/)![0];
        const before = head.slice(0, head.length - word.length).trim().split(/\s+/).filter((w) => w);
        const candidates = await this.completer(word, before);
        if (candidates.length === 0) return this.term.write("\x07");
        if (candidates.length === 1) return insert(candidates[0].slice(word.length) + " ");
        const common = candidates.reduce((a, b) => {
          let i = 0;
          while (i < a.length && a[i] === b[i]) i++;
          return a.slice(0, i);
        });
        if (common.length > word.length) return insert(common.slice(word.length));
        const now = Date.now();
        if (now - lastTab < 800) {
          this.term.write(below() + candidates.join("  ") + "\r\n");
        }
        lastTab = now;
      };

      this.handler = async (key: string) => {
        switch (key) {
          case "\x03": // Ctrl-C
            this.term.write("^C" + below());
            this.flush();
            return finish({ tag: "interrupt" });
          case "\x04": // Ctrl-D
            if (lines.length === 0 && buffer.length === 0) {
              this.term.write(below());
              return finish({ tag: "eof" });
            }
            return;
          case "\r": {
            const input = [...lines, buffer].join("\n");
            this.term.write(below());
            if (await incomplete(input)) {
              lines.push(buffer);
              buffer = "";
              cursor = 0;
              current = continuation;
              render();
              return;
            }
            if (input.trim() !== "" && this.history[0] !== input) this.history.unshift(input);
            return finish({ tag: "line", line: input });
          }
          case "\x7f": // Backspace
          case "\x08":
            if (cursor > 0) {
              buffer = buffer.slice(0, cursor - 1) + buffer.slice(cursor);
              cursor--;
            }
            break;
          case "\x1b[3~": // Delete
            buffer = buffer.slice(0, cursor) + buffer.slice(cursor + 1);
            break;
          case "\x01": // Ctrl-A
          case "\x1b[H":
          case "\x1b[1~":
          case "\x1bOH":
            cursor = 0;
            break;
          case "\x05": // Ctrl-E
          case "\x1b[F":
          case "\x1b[4~":
          case "\x1bOF":
            cursor = buffer.length;
            break;
          case "\x0b": // Ctrl-K
            buffer = buffer.slice(0, cursor);
            break;
          case "\x15": // Ctrl-U
            buffer = buffer.slice(cursor);
            cursor = 0;
            break;
          case "\x17": { // Ctrl-W
            const head = buffer.slice(0, cursor).replace(/\S*\s*$/, "");
            buffer = head + buffer.slice(cursor);
            cursor = head.length;
            break;
          }
          case "\x0c": // Ctrl-L
            this.term.write("\x1b[2J\x1b[H");
            break;
          case "\t":
            await complete();
            break;
          case "\x1b[D": // Left
          case "\x02":
            if (cursor > 0) cursor--;
            break;
          case "\x1b[C": // Right
          case "\x06":
            if (cursor < buffer.length) cursor++;
            break;
          case "\x1bb": // Alt-Left, word back
          case "\x1b[1;3D":
            cursor = buffer.slice(0, cursor).replace(/\S+\s*$/, "").length;
            break;
          case "\x1bf": // Alt-Right, word forward
          case "\x1b[1;3C": {
            const rest = buffer.slice(cursor).match(/^\s*\S+/);
            cursor += rest ? rest[0].length : buffer.length - cursor;
            break;
          }
          case "\x1b[A": // Up, older history
          case "\x10":
            if (this.history.length === 0) break;
            if (historyIndex === null) {
              saved = buffer;
              historyIndex = 0;
            } else historyIndex = Math.min(historyIndex + 1, this.history.length - 1);
            buffer = flatten(this.history[historyIndex]);
            cursor = buffer.length;
            break;
          case "\x1b[B": // Down, newer history
          case "\x0e":
            if (historyIndex === null) break;
            if (historyIndex === 0) {
              historyIndex = null;
              buffer = saved;
            } else buffer = flatten(this.history[--historyIndex]);
            cursor = buffer.length;
            break;
          default:
            if (key.charCodeAt(0) < 32 || key.charCodeAt(0) === 127) return; // other control keys
            insert(key);
        }
        if (!["\x1b[A", "\x1b[B", "\x10", "\x0e"].includes(key)) historyIndex = null;
        render();
      };

      render();
      this.drain();
    });
  }
}
