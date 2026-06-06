import { Component } from "preact";
import { html } from "@/html.ts";

export class Footer extends Component {
  override render() {
    return html`
      <footer class="footer">
        The IMP Language Interpreter in the browser! Made with ${"<3"} by Basil
        Feitknecht.
      </footer>
    `;
  }
}
