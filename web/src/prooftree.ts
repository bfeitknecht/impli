import { $typst } from "npm:@myriaddreamin/typst.ts@0.7.0/contrib/snippet";
import type { RuleJson } from "@/protocol.ts";

// The derivation tree is typeset with Typst (prooftrees from the vendored
// curryst package, same macro the paper uses) and embedded as inline SVG.
// The compiler and renderer WASM (cached by the service worker/CDN) are
// fetched lazily the first time a tree is requested.

let initialization: Promise<void> | null = null;

/** Lazily initialize the Typst compiler and its font/prooftree providers. */
async function init(): Promise<void> {
  if (!initialization) {
    $typst.setCompilerInitOptions({
      getModule: () => "./typst/typst_ts_web_compiler_bg.wasm",
    });
    $typst.setRendererInitOptions({
      getModule: () => "./typst/typst_ts_renderer_bg.wasm",
    });
    initialization = (async () => {
      const lib = await fetch("./curryst.typ");
      if (!lib.ok) throw new Error(`failed to load curryst: ${lib.status}`);
      await $typst.addSource("/curryst.typ", await lib.text());
    })();
  }
  return initialization;
}

/** Colors for the tree, taken from the active terminal color scheme. */
interface TreeColors {
  foreground: string;
  name: string;
}

function palette(): TreeColors {
  const getVar = (name: string) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  return {
    foreground: getVar("--terminal-fg") || "#000000",
    name: getVar("--ansi-green") || "#4e9a06",
  };
}

/** Escape a typst string literal. */
function quoted(text: string): string {
  return text.replaceAll("\\", "\\\\").replaceAll('"', '\\"');
}

/** Escape typst content text. */
function escaped(text: string): string {
  return text
    .replaceAll("\\", "\\\\")
    .replaceAll("#", "\\#")
    .replaceAll("[", "\\[")
    .replaceAll("]", "\\]");
}

/** `x := 1, y := 2` → `x |-> 1, y |-> 2` */
function bindings(list: string): string {
  return list
    .split(",")
    .map((part) => part.trim())
    .filter((part) => part !== "")
    .map((part) => part.replace(":=", "|->"))
    .join(", ");
}

/**
 * Typeset an ASCII conclusion `< s, {bindings} > -> {bindings}` as the
 * paper's `⟨ s, σ ⟩ → σ′` with set-update notation. Falls back to upright
 * text when the shape is not recognized.
 */
function conclusion(source: string): string {
  const match = /^< (.*), \{(.*)\} > -> \{(.*)\}$/.exec(source);
  if (!match) return `[#mono("${quoted(source)}")]`;
  const [, stm, before, after] = match;
  const left = before === "" ? "sigma" : `sigma[${bindings(before)}]`;
  const right = after === "" ? "sigma'" : `sigma'[${bindings(after)}]`;
  return `[$chevron.l mono("${quoted(stm)}"), ${left} chevron.r -> ${right}$]`;
}

/** Emit a curryst `rule(...)` for the derivation node. */
function rule(node: RuleJson): string {
  const premises = node.premise.map((child) => `${rule(child)},`).join("\n");
  return `rule(
  name: [#text(fill: name-color, size: 0.85em)[${escaped(node.name)}]],
  ${premises}
  ${conclusion(node.conclusion)},
)`;
}

/** Render the derivation tree to an SVG fragment for inline embedding. */
export async function renderTree(root: RuleJson): Promise<string> {
  await init();
  const colors = palette();
  const source = `#import "/curryst.typ": prooftree, rule
#set page(width: auto, height: auto, margin: (x: 12pt, y: 8pt), fill: none)
#set text(fill: rgb("${colors.foreground}"), size: 10pt)
#let name-color = rgb("${colors.name}")
#let mono(it) = text(font: "DejaVu Sans Mono", size: 0.9em, raw(it))
#align(center, prooftree(
${rule(root)}
))
`;
  return await $typst.svg({ mainContent: source });
}

/** Drop the Typst compiler so a palette change recompiles freshly. */
export function retheme() {
  initialization = null;
}
