// import { Terminal } from "xterm";
// import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory, Fd } from "shim";
import { Terminal } from "npm:@xterm/xterm";
import { WASI, File, OpenFile, ConsoleStdout, Fd } from "npm:@bjorn3/browser_wasi_shim";

export class Stdio extends ConsoleStdout {
  pty: Terminal;
  file_pos = 0;
  constructor(pty: Terminal) {
    super(pty.write);
    this.pty = pty;
  }

  //  pty.onRead
  // override fd_read(data: Uint8Array, iovs?: [Iovec]) {
  //   let nread = 0;
  //   for (const iovec of iovs!) {
  //     if (this.file_pos < this.file.data.byteLength) {
  //       const slice = this.file.data.slice(
  //         Number(this.file_pos),
  //         Number(this.file_pos + iovec.buf_len),
  //       );
  //       data.set(slice, iovec.buf);
  //       this.file_pos += slice.length;
  //       nread += slice.length;
  //     } else {
  //       break;
  //     }
  //   }
  //   return { ret: 0, nread };
  // }

  lineBuffered(): Stdio {
    const decoder = new TextDecoder("utf-8", { fatal: false });
    const encoder = new TextEncoder();
    let line_buf = "";
    const _write = this.write;
    this.write = (buffer) => {
      line_buf += decoder.decode(buffer, { stream: true });
      const lines = line_buf.split("\n");
      for (const [i, line] of lines.entries()) {
        if (i < lines.length - 1) {
          _write(encoder.encode(line));
        } else line_buf = line;
      }
    };
    return this;
  }
}
