import { h } from "preact";
import htm from "htm";
import type { AstNode } from "@/protocol.ts";

const html = (htm as any).bind(h);

const SCALAR_FIELDS: { [tag: string]: string[] } = {
  val: ["value"],
  var: ["name"],
  vardef: ["var", "op"],
  seq: [],
  ifelse: [],
  while: [],
  print: [],
  read: ["var"],
  local: ["var"],
  par: [],
  nondet: [],
  procdef: ["name", "params", "returns"],
  procinvoc: ["name", "returns"],
  restore: [],
  return: ["rets", "returns"],
  break: [],
  revert: [],
  match: [],
  havoc: ["var"],
  assert: [],
  flipflop: ["index"],
  raise: [],
  trycatch: ["var"],
  swap: ["left", "right"],
  timeout: [],
  alternate: [],
  bin: ["op"],
  lit: ["value"],
  or: [],
  and: [],
  not: [],
  rel: ["op"],
};

function isNode(value: unknown): value is AstNode {
  return (
    typeof value === "object" &&
    value !== null &&
    typeof (value as AstNode).tag === "string"
  );
}

/** Child fields of a node in display order. */
function childFields(node: AstNode): [string, unknown][] {
  const entries: [string, unknown][] = [];
  for (const [field, value] of Object.entries(node)) {
    if (field === "tag") continue;
    if (isNode(value)) entries.push([field, value]);
    else if (Array.isArray(value) && value.length > 0 && value.every(isNode)) {
      entries.push([field, value]);
    } else if (field === "branches" && Array.isArray(value) && value.length > 0) {
      const branches = (value as { value: number; statement: AstNode }[]).filter(
        (branch) => isNode(branch.statement),
      );
      if (branches.length > 0) entries.push([field, branches]);
    }
  }
  return entries;
}

function scalarSummary(node: AstNode): string {
  const fields = SCALAR_FIELDS[node.tag] ?? [];
  const scalars = fields
    .filter((field) => node[field] !== undefined && !isNode(node[field]))
    .map((field) => JSON.stringify(node[field]));
  return scalars.join(" ");
}

export function AstTree({ node }: { node: AstNode }) {
  const children = childFields(node);
  const scalars = scalarSummary(node);
  if (children.length === 0) {
    return html`
      <div class="ast-leaf">
        <span class="ast-tag">${node.tag}</span>
        ${scalars && html`<span class="ast-scalar">${scalars}</span>`}
      </div>
    `;
  }
  return html`
    <details class="ast-node" open>
      <summary>
        <span class="ast-tag">${node.tag}</span>
        ${scalars && html`<span class="ast-scalar">${scalars}</span>`}
      </summary>
      <div class="ast-children">
        ${children.map(([field, value]) => {
          const nodes = Array.isArray(value) ? value : [value];
          if (field === "branches") {
            return nodes.length === 0
              ? null
              : html`
                  <div class="ast-field">
                    <span class="ast-field-name">${field}</span>
                    ${(value as { value: number; statement: AstNode }[]).map(
                      (branch) => html`
                        <div class="ast-field-child">
                          <span class="ast-scalar">${branch.value}:</span>
                          <${AstTree} node=${branch.statement} />
                        </div>
                      `,
                    )}
                  </div>
                `;
          }
          return html`
            <div class="ast-field">
              <span class="ast-field-name">${field}</span>
              ${nodes.map(
                (child) =>
                  html`
                    <div class="ast-field-child">
                      <${AstTree} node=${child as AstNode} />
                    </div>
                  `,
              )}
            </div>
          `;
        })}
      </div>
    </details>
  `;
}
