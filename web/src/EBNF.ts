import { Component } from "preact";
import { html } from "@/html.ts";

export class EBNF extends Component {
  override render() {
    return html`
      <div class="placeholder">
        EBNF grammar visualization not yet implemented.
      </div>
    `;
  }
}

/*
IDEA
- copy of https://thomasgassmann.com/ebnf
- verify / produce words
- visualize EBNF in diagram
- automatically view current REPL commandline if not empty

TOOLS
- CodeMirror editor with live feedback

*/
