import { Component, createRef, render } from "preact";
import { Unsupported } from "@/Unsupported.tsx";
import { Impli } from "@/impli.ts";

export class App extends Component {
  private containerRef = createRef<HTMLDivElement>();
  private impli: Impli | null = null;

  override componentDidMount() {
    if (this.containerRef.current) {
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
      return <Unsupported />;
    }

    return <div id="terminal" ref={this.containerRef} />;
  }
}

render(<App />, document.body);
