// Impli subclass of WasmWebTerm that auto-launches impli REPL
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

  // Disable xterm prompt
  _xtermPrompt() {
    return "";
  }

  // Disable line splitting and pass input to WebWorker
  runLine(line) {
    super._setStdinBuffer(line);
  }

  // Custom REPL
  async repl() {
    // Read
    const line = await this._xtermEcho.read("");
    this._xterm.write("\r\n");
    if (line.trim() == "") return this.repl();

    // Evaluate
    this.runLine(line);

    // Print
    // WASM impli REPL automatically prints output
    if (this._outputBuffer.slice(-1) != "\n") this._xterm.write("\u23CE\r\n");
    this._xterm.write("\r\n");

    // Loop
    this.repl();
  }

  // Start impli REPL
  async activate(xterm) {
    // Set up addons, registers JS commands
    await super.activate(xterm);

    // Run WASM impli REPL
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
