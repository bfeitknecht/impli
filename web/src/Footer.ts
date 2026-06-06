import { Component } from "preact";
import { html } from "@/html.ts";

export class Footer extends Component {
  override render() {
    return html`
      <footer class="footer">
        <span>The IMP Language Interpreter in the browser!</span>
        <br />
        <span>Made with ${"<3"} by Basil Feitknecht.</span>
      </footer>
    `;
  }
}
