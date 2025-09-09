import { wasi, Fd, ConsoleStdout } from "./lib/shim.js";

export class Stdin extends Fd {
  constructor(pty) {
    super();
    this.pty = pty;
    this.encoder = new TextEncoder();
    this.inputQueue = [];

    this.pty.onReadable(() => {
      const data = this.pty.read();
      if (data) {
        this.#handlePtyInput(data);
      }
    });
  }

  #handlePtyInput(data) {
    const chunk = this.encoder.encode(data);
    console.log(`PTY Input for stdin: "${data}"`, chunk);
    this.inputQueue.push(chunk);
  }

  fd_fdstat_get() {
    const fdstat = new wasi.Fdstat(wasi.FILETYPE_CHARACTER_DEVICE, 0);
    return { ret: wasi.ERRNO_SUCCESS, fdstat };
  }

  fd_read(size) {
    if (this.inputQueue.length === 0) {
      return { ret: wasi.ERRNO_AGAIN, data: new Uint8Array() };
    }

    const chunk = this.inputQueue[0];
    if (size >= chunk.length) {
      this.inputQueue.shift();
      return { ret: wasi.ERRNO_SUCCESS, data: chunk };
    } else {
      const data = chunk.subarray(0, size);
      this.inputQueue[0] = chunk.subarray(size);
      return { ret: wasi.ERRNO_SUCCESS, data };
    }
  }

  poll_oneoff(subscriptions) {
    let nsubscriptions = 0;
    const events = [];

    for (const sub of subscriptions) {
      if (sub.type === wasi.EVENTTYPE_FD_READ && sub.u.fd_readwrite.fd === 0) {
        if (this.inputQueue.length > 0) {
          const nbytes = this.inputQueue.reduce((acc, chunk) => acc + chunk.length, 0);
          const event = {
            userdata: sub.userdata,
            type: wasi.EVENTTYPE_FD_READ,
            error: wasi.ERRNO_SUCCESS,
            u: { fd_readwrite: { nbytes, flags: 0 } },
          };
          events.push(event);
          nsubscriptions++;
        }
      }
    }

    return { ret: wasi.ERRNO_SUCCESS, nsubscriptions, events };
  }
}

export class Stdout extends ConsoleStdout {
  constructor(pty) {
    const decoder = new TextDecoder("utf-8", { fatal: false });
    let line_buf = "";
    const write = (buffer) => {
      line_buf += decoder.decode(buffer, { stream: true });
      const lines = line_buf.split("\n");
      for (const [i, line] of lines.entries()) {
        if (i != lines.length - 1) {
          pty.write(line + "\r\n");
        } else {
          line_buf = line;
          pty.write(line);
        }
      }
    };
    super(write);
  }
}
