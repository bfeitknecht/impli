import { Component, render } from "preact";
import { Unsupported } from "@/Unsupported.tsx";
import { Impli } from "@/impli.ts";

export class App extends Component {
  override render() {
    // TODO: Check if ServiceWorker and WebAssembly are supported
    if (true) {
      return Unsupported;
    }
  }
}

render(<App />, document.body);
