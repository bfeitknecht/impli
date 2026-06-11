import { h } from "preact";
import htm from "htm";

// Bind htm to Preact's h once, shared across all components
// deno-lint-ignore no-explicit-any
export const html = (htm as any).bind(h);

// Context injected debug log
const DEBUG = false;
// deno-lint-ignore no-explicit-any
export function log(context: string, ...args: any[]) {
  if (DEBUG) {
    console.log(`[DEBUG] ${context}:`, ...args);
  }
}
