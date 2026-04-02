import { Component, createRef, h, render } from "preact";
import htm from "htm";
import { Unsupported } from "@/Unsupported.ts";
import { Impli } from "@/impli.ts";

// LSP isn't hip enough to know about HTM
const html = (htm as any).bind(h);

export class App extends Component {
  private containerRef = createRef<HTMLDivElement>();
  private impli: Impli | null = null;

  override async componentDidMount() {
    if (this.containerRef.current) {
      if (document.readyState !== "complete") {
        await new Promise((resolve) => {
          globalThis.addEventListener("load", resolve, { once: true });
        });
      }

      await document.fonts.ready;

      this.impli = new Impli(this.containerRef.current);
      this.impli.start();
    }
  }

  override render() {
    const hasWasm = typeof WebAssembly === "object" &&
      typeof WebAssembly.instantiateStreaming === "function";
    const hasBigInt64Array = typeof BigInt64Array === "function";
    const hasGlobalThis = typeof globalThis !== "undefined";

    const supported = hasWasm && hasBigInt64Array && hasGlobalThis;

    if (!supported) {
      return html`
        <${Unsupported} />
      `;
    }

    return html`
      <div id="terminal" ref="${this.containerRef}" />
    `;
  }
}

render(
  html`
    <${App} />
  `,
  document.body,
);
