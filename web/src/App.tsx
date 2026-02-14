import { Component, createRef } from "preact";
import { Impli } from "@/impli.ts";

export class App extends Component {
  private terminalElementRef = createRef<HTMLDivElement>();
  private impli: Impli | null = null;

  componentDidMount() {
    if (this.terminalElementRef.current) {
      // Initialize the Impli terminal application
      this.impli = new Impli(this.terminalElementRef.current);

      // Start the application (Service Worker, WASM, etc.)
      this.impli.start();
    }
  }

  componentWillUnmount() {
    // Cleanup on unmount
    if (this.impli) {
      this.impli.dispose();
      this.impli = null;
    }
  }

  render() {
    return <div ref={this.terminalElementRef} id="terminal" />;
  }
}
