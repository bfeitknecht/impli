// Impli subclass of WasmWebTerm with alternating control:
// - local-echo controls input (editing, navigation, history)
// - WASM controls execution and output only
export class REPL extends WasmWebTerm.default {
  // Nice welcome message
  printWelcomeMessagePlusControlSequences() {
    const dedent = (strings, ...values) => {
      let raw = "";
      for (let i = 0; i < strings.length; i++) {
        raw += strings[i];
        if (i < values.length) raw += values[i];
      }
      const lines = raw.split("\n");
      const normalized = lines.map((line) => line.replace(/^[ \t]+/, ""));
      return normalized.join("\r\n") + "\r\n";
    };

    const logo = dedent`\
      ,_  ,_     ,___  ,_    ,_
      | | | |\\/| | |_) | |   | |
      |_| |_|  | |_|   |_|__ |_|
      `;

    const message = dedent`\
      Execute IMP in the browser and inspect resulting state.
      Made with <3 by Basil Feitknecht`;

    // Bold control characters for logo
    return "\x1bc\x1b[1m" + logo + "\x1b[0m" + message;
  }

  // Disable xterm prompt (WASM will print its own)
  _xtermPrompt() {
    return "";
  }

  // Override repl to do nothing - WASM drives its own REPL loop
  // WasmWebTerm's default activate() tries to start a REPL, we prevent it
  async repl() {
    // Do nothing - the WASM impli binary runs its own REPL
    // It will call _stdinProxy when it needs input
  }

  // Override stdin proxy - WASM asking for input
  // Read from terminal when WASM requests it
  async _stdinProxy(message) {
    console.log("[INFO] WASM requesting input, reading from terminal");

    // Read line from terminal (local-echo provides editing/navigation)
    const line = await this._xtermEcho.read("");

    // Clear the line that local-echo just echoed
    // Move cursor to beginning of line and clear it
    this._xterm.write("\r\x1b[K");

    console.log("[INFO] User entered:", line);

    // Set stdin buffer with the user's input
    this._setStdinBuffer(line + "\n");

    // Resume the worker so it can read from the buffer
    this._resumeWorker();
  }

  // Start impli REPL
  async activate(xterm) {
    // Set up addons, registers JS commands
    await super.activate(xterm);

    // Start the WASM impli REPL
    // WASM will drive the REPL loop and call _stdinProxy when it needs input
    console.log("[INFO] Starting WASM impli REPL");
    await this.runWasmCommand("impli", []);
  }

  // Write impli REPL trace to plaintext blob in new browser tab
  writeTrace(trace) {
    // Create blob from trace
    const blob = new Blob([trace], { type: "text/plain" });

    // Create URL for blob
    const url = URL.createObjectURL(blob);

    // Open blob in new tab
    globalThis.open(url, "_blank");

    // Revoke URL after delay to free memory
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }
}
