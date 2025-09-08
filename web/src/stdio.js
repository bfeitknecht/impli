import { ConsoleStdout, wasi } from "shim";

export class Stdio extends ConsoleStdout {
  constructor(pty) {
    super(pty.write);
    this.pty = pty;
    this.read = () => new Uint8Array();
    this.write = pty.write;
  }

  fd_read(size) {
    this.pty.onReadable(() => {
      const input = Uint8Array.from(this.pty.read());
      if (size <= input.length) {
        return { ret: 0, data: input.subarray(0, size) };
      } else return { ret: wasi.ERRNO_2BIG, data: new Uint8Array() };
    });
    return { ret: wasi.ERRNO_2BIG, data: new Uint8Array() };
  }
}
