import { Component } from "preact";
import { html } from "@/html.ts";
import * as railroad from "railroad-diagrams";
import ebnf from "ebnf" with { type: "json" };

interface Rule {
  name: string;
  comment: string;
}

export class EBNF extends Component {
  override render() {
    return html`
      <div class="placeholder">
        EBNF grammar visualization not yet implemented.
      </div>
    `;
  }
}
