import { Component } from "preact";
import { html } from "@/html.ts";

export class EBNF extends Component {
  override render() {
    return html`
      <div class="placeholder">
        <div class="placeholder-prompt">
          <span class="placeholder-ps1">impli$</span>
          <span class="placeholder-cmd"> ebnf</span>
        </div>
        <div class="placeholder-output">
          <span class="placeholder-todo">[TODO]</span> EBNF grammar visualization not
          yet implemented.
        </div>
      </div>
    `;
  }
}
