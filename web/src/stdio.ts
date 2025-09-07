import { Terminal } from "xterm";
import { ConsoleStdout, ERRNO_2BIG } from "shim";
// import { Terminal } from "npm:@xterm/xterm";
// import { WASI, File, OpenFile, ConsoleStdout, Fd } from "npm:@bjorn3/browser_wasi_shim";

export class Stdio extends ConsoleStdout {
  pty: Terminal;
  read: () => Uint8Array;
  write: (buffer: Uint8Array) => void;

  constructor(pty: Terminal) {
    super(pty.write);
    this.pty = pty;
    this.read = () => new Uint8Array();
    this.write = pty.write;
  }

  fd_read(size: number) {
    this.pty.onReadable(() => {
      const input = Uint8Array.from(this.pty.read());
      if (size <= input.length) {
        return { ret: 0, data: input.subarray(0, size) };
      } else return { ret: ERRNO_2BIG, data: new Uint8Array() };
    });
    return { ret: ERRNO_2BIG, data: new Uint8Array() };
  }
}
