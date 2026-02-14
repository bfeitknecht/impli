import { Component, createRef, render } from "preact";
import { Impli } from "@/impli.ts";
import { checkRequirements, Unsupported } from "@/Unsupported.tsx";

export class App
  extends Component<{}, { check: ReturnType<typeof checkRequirements> }> {
  private terminalElementRef = createRef<HTMLDivElement>();
  private impli: Impli | null = null;

  constructor() {
    super();
    this.state = {
      check: checkRequirements(),
    };
  }

  override componentDidMount() {
    const { check } = this.state;
    // Allow start if supported or if Service Worker can potentially fix isolation
    if (check.supported || check.features["Service Workers"]) {
      if (this.terminalElementRef.current) {
        // Initialize the Impli terminal application
        this.impli = new Impli(this.terminalElementRef.current);

        // Start the application (Service Worker, WASM, etc.)
        this.impli.start();
      }
    }
  }

  override componentWillUnmount() {
    // Cleanup on unmount
    if (this.impli) {
      this.impli.dispose();
      this.impli = null;
    }
  }

  override render() {
    const { check } = this.state;

    // Display error only if features are missing and we can't fix it with SW
    if (!check.supported && !check.features["Service Workers"]) {
      return <Unsupported check={check} />;
    }

    return <div ref={this.terminalElementRef} id="terminal" />;
  }
}

render(<App />, document.body);
