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

export function overrideWasiPoller(wasiInstance) {
  const original_poll_oneoff = wasiInstance.wasiImport.poll_oneoff;

  wasiInstance.wasiImport.poll_oneoff = (in_ptr, out_ptr, nsubscriptions, nevents_ptr) => {
    const memory = new DataView(wasiInstance.inst.exports.memory.buffer);
    const subscriptions = [];
    for (let i = 0; i < nsubscriptions; i++) {
      subscriptions.push(
        wasi.Subscription.read_bytes(memory, in_ptr + i * 48 /* sizeof(subscription) */),
      );
    }

    // Group subscriptions by file descriptor
    const subsByFd = new Map();
    for (const sub of subscriptions) {
      const fd = sub.u.fd_readwrite.fd;
      if (!subsByFd.has(fd)) {
        subsByFd.set(fd, []);
      }
      subsByFd.get(fd).push(sub);
    }

    let allEvents = [];
    let totalNsubscriptions = 0;

    // Delegate to each Fd object's poll_oneoff method
    for (const [fd, subs] of subsByFd.entries()) {
      const fd_obj = wasiInstance.fds[fd];
      if (fd_obj && typeof fd_obj.poll_oneoff === "function") {
        const { ret, nsubscriptions: count, events } = fd_obj.poll_oneoff(subs);
        if (ret === wasi.ERRNO_SUCCESS) {
          totalNsubscriptions += count;
          allEvents.push(...events);
        }
      }
    }

    // Write events back to memory
    if (totalNsubscriptions > 0) {
      memory.setUint32(nevents_ptr, totalNsubscriptions, true);
      for (let i = 0; i < totalNsubscriptions; i++) {
        allEvents[i].write_bytes(memory, out_ptr + i * 32 /* sizeof(event) */);
      }
    } else {
      memory.setUint32(nevents_ptr, 0, true);
    }

    return wasi.ERRNO_SUCCESS;
  };
}
