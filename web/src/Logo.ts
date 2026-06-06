import { Component } from "preact";
import { html } from "@/html.ts";

const text = `\
o  _ _   _   ) o
( ) ) ) )_) (  (
       (`;

export class Logo extends Component {
  override render() {
    return html`
      <pre class="logo">${text}</pre>
    `;
  }
}
