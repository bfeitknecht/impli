import { h } from "preact";
import htm from "htm";

// Bind htm to Preact's h once, shared across all components
// deno-lint-ignore no-explicit-any
export const html = (htm as any).bind(h);
