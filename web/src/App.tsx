import { Component, render } from "preact";
import { Unsupported } from "@/Unsupported.tsx";
import { Impli } from "@/impli.ts";

export class App extends Component {
  override render() {
    // TODO: Check if ServiceWorker and WebAssembly are supported
    const old = false;
    if (old) {
      return <Unsupported />;
    }

    return <>Hello, World!</>;
  }
}

render(<App />, document.body);
