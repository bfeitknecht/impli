/** Messages between the page and the interpreter worker. */

/** Page to worker. */
export type Request =
  | { type: "submit"; line: string }
  | { type: "answer"; value: string }
  | { type: "cancel" }
  | { type: "incomplete"; id: number; input: string }
  | { type: "complete"; id: number; prefix: string };

/** Worker to page. */
export type Response =
  | { type: "ready" }
  | { type: "output"; text: string }
  | { type: "clear" }
  | { type: "ask"; prompt: string }
  | { type: "save"; path: string; content: string }
  | { type: "done" }
  | { type: "reply"; id: number; value: unknown }
  | { type: "failed"; message: string };
