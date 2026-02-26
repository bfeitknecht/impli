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
    // TODO: Check if ServiceWorker and WebAssembly are supported
    const old = false;
    if (old) {
      return <Unsupported />;
    }

    return <div id="terminal" ref={this.containerRef} />;
  }
}

render(<App />, document.body);
