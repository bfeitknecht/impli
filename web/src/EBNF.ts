import { Component } from "preact";
import { html, log } from "@/html.ts";
import * as railroad from "railroad-diagrams";

export class EBNF extends Component {
  override render() {
    return html`
      <div class="placeholder">
        EBNF grammar visualization not yet implemented.
      </div>
    `;
  }
}
