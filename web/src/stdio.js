import { ConsoleStdout, wasi } from "shim";

export class Stdio extends ConsoleStdout {
  constructor(pty) {
    const decoder = new TextDecoder("utf-8", { fatal: false });
    let line_buf = "";
    const write = (buffer) => {
      line_buf += decoder.decode(buffer, { stream: true });
      const lines = line_buf.split("\n");
      console.log(lines.entries());
      for (const [i, line] of lines.entries()) {
        if (i != lines.length - 1) {
          pty.write(line + "\n");
        } else {
          line_buf = line;
          pty.write(line);
        }
      }
    };
    super(write);
    this.pty = pty;
  }

  fd_read(size) {
    console.log("fd_read: called");

    const data = new Uint8Array();
    this.pty.onReadable(() => {
      console.log("fd_read: pty.onReadable()");
      const input = this.pty.read();
      const view8 = Uint8Array.from(input);

      console.log("fd_read: pty.read() -> " + input);
      console.log("fd_read: view8.length = " + view8.length);
      console.log("fd_read: size = " + size);

      // this.pty.write(input);

      if (view8.length <= size) {
        return { ret: 0, data: view8.subarray(0, size) };
      }
    });
    return { ret: wasi.ERRNO_2BIG, data: data };
  }
}
