export class Terminal {
  constructor() {
    this.in = "";
    this.prompt = "IMP> ";
    this.welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands.";
    this.terminal = new globalThis.Terminal({
      cursorBlink: true,
    });
    this.terminal.open(document.getElementById("terminal"));
    this.terminal.write(this.welcome);
  }

  write(str) {
    this.terminal && this.terminal.write(str + "\r\n");
  }
}
