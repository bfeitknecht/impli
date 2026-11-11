/**
 * Bespoke line reader for the ghostty-web terminal, replacing the
 * xterm-local-echo addon: line editing with cursor movement and history,
 * tab completion hooks, bracket-balance multi-line continuation, and an
 * explicit Ctrl-C/Ctrl-D result protocol instead of thrown aborts.
 *
 * Only the features the IMP frontend actually needs, nothing else.
 */

/** Structural subset of the ghostty-web terminal the reader relies on. */
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

/**
 * Tab completion hook: given the index of the token under the cursor and
 * all tokens of the line, return candidate completions. The first handler
 * returning a non-empty list wins.
 */
export type CompletionHandler = (index: number, tokens: string[]) => string[];

/** Whether accumulated input is incomplete and continues on the next line. */
export type IncompleteHandler = (line: string) => boolean;

/** Default incompleteness check: unbalanced brackets or braces. */
export function bracketsOpen(line: string): boolean {
  let depth = 0;
  for (const char of line) {
    if ("([{".includes(char)) depth++;
    if (")]}".includes(char)) depth--;
    if (depth < 0) return false; // overclosed, treat as complete
  }
  return depth > 0;
}

export class Readline {
  private term: ReadlineTerminal;
  private history: string[] = [];
  private completionHandlers: CompletionHandler[] = [];
  private isIncomplete: IncompleteHandler;

  constructor(term: ReadlineTerminal, isIncomplete: IncompleteHandler = bracketsOpen) {
    this.term = term;
    this.isIncomplete = isIncomplete;
  }

  public addCompletion(handler: CompletionHandler) {
    this.completionHandlers.push(handler);
  }

  /** Print with newline normalization, matching terminal carriage semantics. */
  public print(text: string) {
    this.term.write(text.replace(/\r?\n/g, "\r\n"));
  }

  /** Read one logical line, continuing while input is incomplete. */
  public read(prompt: string, continuationPrompt = "+ "): Promise<ReadResult> {
    return new Promise((resolve) => {
      const lines: string[] = [];
      let buffer = "";
      let cursor = 0;
      let currentPrompt = prompt;
      // History navigation state: index into history plus saved edit line
      let historyIndex: number | null = null;
      let savedEdit = "";
      // Double-tab candidate listing state
      let lastTabAt = 0;

      const posOf = (k: number): [number, number] => {
        const cols = Math.max(1, this.term.cols);
        if (k === 0) return [0, 0];
        if (k % cols === 0) return [Math.floor((k - 1) / cols), cols - 1]; // pending wrap
        return [Math.floor(k / cols), k % cols];
      };

      const render = () => {
        const text = currentPrompt + buffer;
        this.term.write("\r\x1b[0J" + text);
        const [endRow] = posOf(text.length);
        const [targetRow, targetCol] = posOf(currentPrompt.length + cursor);
        let move = "";
        if (endRow > targetRow) move += `\x1b[${endRow - targetRow}A`;
        move += "\r";
        if (targetCol > 0) move += `\x1b[${targetCol}C`;
        if (move) this.term.write(move);
      };

      const insert = (text: string) => {
        for (const char of text) {
          if (char < " ") continue; // strip control bytes of pastes
          buffer = buffer.slice(0, cursor) + char + buffer.slice(cursor);
          cursor++;
        }
      };

      const complete = () => {
        const head = buffer.slice(0, cursor);
        const tokens = head.split(/\s+/).filter((t) => t.length > 0);
        const endsWithSpace = /\s$/.test(head);
        // Token under the cursor: last one, or a fresh empty token after space
        const index = endsWithSpace ? tokens.length : Math.max(0, tokens.length - 1);
        const current = endsWithSpace ? "" : (tokens[index] ?? "");
        for (const handler of this.completionHandlers) {
          const candidates = handler(index, tokens).filter((c) => c.startsWith(current));
          if (candidates.length === 0) continue;
          if (candidates.length === 1) {
            insert(candidates[0].slice(current.length) + " ");
            return;
          }
          const common = candidates.reduce((a, b) => {
            let i = 0;
            while (i < a.length && a[i] === b[i]) i++;
            return a.slice(0, i);
          });
          if (common.length > current.length) {
            insert(common.slice(current.length));
            lastTabAt = Date.now();
            return;
          }
          const now = Date.now();
          if (now - lastTabAt < 1000) {
            this.term.write("\r\n" + candidates.join("  ") + "\r\n");
            render();
          }
          lastTabAt = now;
          return;
        }
        this.term.write("\x07");
      };

      const listener = this.term.onData((data) => {
        // Strip bracketed paste markers
        const chunk = data.replace(/\x1b\[2(?:0[01])~/g, "");
        if (chunk.length === 0) return;
        switch (chunk) {
          case "\x03": // Ctrl-C
            listener.dispose();
            this.term.write("^C\r\n");
            resolve({ tag: "interrupt" });
            return;
          case "\x04": // Ctrl-D
            if (lines.length === 0 && buffer.length === 0) {
              listener.dispose();
              resolve({ tag: "eof" });
            }
            return;
          case "\r": // Enter
          case "\n": {
            const accumulated = [...lines, buffer].join("\n");
            if (this.isIncomplete(accumulated)) {
              lines.push(buffer);
              buffer = "";
              cursor = 0;
              currentPrompt = continuationPrompt;
              this.term.write("\r\n");
              render();
              return;
            }
            listener.dispose();
            this.term.write("\r\n");
            if (accumulated.trim() !== "" && this.history[0] !== accumulated) {
              this.history.unshift(accumulated);
            }
            resolve({ tag: "line", line: accumulated });
            return;
          }
          case "\x7f": // Backspace
            if (cursor > 0) {
              buffer = buffer.slice(0, cursor - 1) + buffer.slice(cursor);
              cursor--;
            }
            break;
          case "\x01": // Ctrl-A
            cursor = 0;
            break;
          case "\x05": // Ctrl-E
            cursor = buffer.length;
            break;
          case "\x0b": // Ctrl-K
            buffer = buffer.slice(0, cursor);
            break;
          case "\x15": // Ctrl-U
            buffer = "";
            cursor = 0;
            break;
          case "\x17": // Ctrl-W
            while (cursor > 0) {
              const before = buffer.slice(0, cursor);
              const trimmed = before.replace(/\s+\S+$/, "");
              if (trimmed !== before) {
                buffer = trimmed + buffer.slice(cursor);
                cursor = trimmed.length;
                break;
              }
              buffer = buffer.slice(0, cursor - 1) + buffer.slice(cursor);
              cursor--;
            }
            break;
          case "\x0c": // Ctrl-L
            this.term.write("\x1b[2J\x1b[H");
            break;
          case "\t": // Tab
            complete();
            break;
          case "\x1b[D": // Left
            if (cursor > 0) cursor--;
            break;
          case "\x1b[C": // Right
            if (cursor < buffer.length) cursor++;
            break;
          case "\x1b[H": // Home
          case "\x1b[1~":
            cursor = 0;
            break;
          case "\x1b[F": // End
          case "\x1b[4~":
            cursor = buffer.length;
            break;
          case "\x1b[A": { // Up: older history
            if (this.history.length === 0) break;
            if (historyIndex === null) {
              savedEdit = buffer;
              historyIndex = 0;
            } else {
              historyIndex = Math.min(historyIndex + 1, this.history.length - 1);
            }
            buffer = this.history[historyIndex];
            cursor = buffer.length;
            break;
          }
          case "\x1b[B": { // Down: newer history
            if (historyIndex === null) break;
            if (historyIndex === 0) {
              historyIndex = null;
              buffer = savedEdit;
            } else {
              historyIndex--;
              buffer = this.history[historyIndex];
            }
            cursor = buffer.length;
            break;
          }
          default: {
            if (chunk.length > 1 && chunk.charCodeAt(0) === 27) break; // ignore other escape sequences
            insert(chunk);
          }
        }
        if (chunk !== "\x1b[A" && chunk !== "\x1b[B") historyIndex = null; // editing leaves history browsing
        render();
      });

      render();
    });
  }
}
