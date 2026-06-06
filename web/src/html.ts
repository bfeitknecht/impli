import { h } from "preact";
import htm from "htm";

// Bind htm to Preact's h once, shared across all components
export const html = (htm as any).bind(h);
