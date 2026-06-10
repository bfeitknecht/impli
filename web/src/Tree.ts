import { Component } from "preact";
import { html } from "@/html.ts";

export class Tree extends Component {
  override render() {
    return html`
      <div class="placeholder">
        Inference tree visualization not yet implemented.
      </div>
    `;
  }
}

/*
IDEA
- visualize inference rule tree
- use canonical Typst rules
- automatically view current REPL commandline if not empty

TOOLS
- typst.ts
*/
