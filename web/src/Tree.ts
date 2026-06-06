import { Component } from "preact";
import { html } from "@/html.ts";

export class Tree extends Component {
  override render() {
    return html`
      <div class="placeholder">
        <div class="placeholder-prompt">
          <span class="placeholder-ps1">impli$</span>
          <span class="placeholder-cmd"> tree</span>
        </div>
        <div class="placeholder-output">
          <span class="placeholder-todo">[TODO]</span> Inference tree visualization
          not yet implemented.
        </div>
      </div>
    `;
  }
}
