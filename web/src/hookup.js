import { wasi } from "shim";
const ERRNO_INVAL = wasi.ERRNO_INVAL;

export function hookup(wasi, pty) {
  const _fd_read = wasi.wasiImport.fd_read;
  wasi.wasiImport.fd_read = (fd, iovs_ptr, iovs_len, nread_ptr) => {
    if (fd == 0) {
      const buffer = new DataView(wasi.inst.exports.memory.buffer);
      const buffer8 = new Uint8Array(wasi.inst.exports.memory.buffer);
      const iovecs = Iovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
      let nread = 0;
      for (i = 0; i < iovecs.length; i++) {
        const iovec = iovecs[i];
        if (iovec.buf_len == 0) {
          continue;
        }
        const data = pty.onRead(iovec.buf_len);
        buffer8.set(data, iovec.buf);
        nread += data.length;
      }
      buffer.setUint32(nread_ptr, nread, true);
      return 0;
    } else {
      console.log("fd_read: unknown fd " + fd);
      return _fd_read.apply(wasi.wasiImport, [fd, iovs_ptr, iovs_len, nread_ptr]);
    }
  };
  const _fd_write = wasi.wasiImport.fd_write;
  wasi.wasiImport.fd_write = (fd, iovs_ptr, iovs_len, nwritten_ptr) => {
    if (fd == 1 || fd == 2) {
      const buffer = new DataView(wasi.inst.exports.memory.buffer);
      const buffer8 = new Uint8Array(wasi.inst.exports.memory.buffer);
      const iovecs = Ciovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
      let wtotal = 0;
      for (i = 0; i < iovecs.length; i++) {
        const iovec = iovecs[i];
        const buf = buffer8.slice(iovec.buf, iovec.buf + iovec.buf_len);
        if (buf.length == 0) {
          continue;
        }
        pty.onWrite(Array.from(buf));
        wtotal += buf.length;
      }
      buffer.setUint32(nwritten_ptr, wtotal, true);
      return 0;
    } else {
      console.log("fd_write: unknown fd " + fd);
      return _fd_write.apply(wasi.wasiImport, [fd, iovs_ptr, iovs_len, nwritten_ptr]);
    }
  };
  wasi.wasiImport.poll_oneoff = (in_ptr, out_ptr, nsubscriptions, nevents_ptr) => {
    if (nsubscriptions == 0) {
      return ERRNO_INVAL;
    }
    const buffer = new DataView(wasi.inst.exports.memory.buffer);
    const in_ = Subscription.read_bytes_array(buffer, in_ptr, nsubscriptions);
    let isReadPollStdin = false;
    let isReadPollConn = false;
    let isClockPoll = false;
    let pollSubStdin;
    let pollSubConn;
    let clockSub;
    let timeout = Number.MAX_VALUE;
    for (const sub of in_) {
      if (sub.u.tag.constiant == "fd_read") {
        if (sub.u.data.fd != 0 && sub.u.data.fd != connfd) {
          console.log("poll_oneoff: unknown fd " + sub.u.data.fd);
          return ERRNO_INVAL; // only fd=0 and connfd is supported as of now (FIXME)
        }
        if (sub.u.data.fd == 0) {
          isReadPollStdin = true;
          pollSubStdin = sub;
        } else {
          isReadPollConn = true;
          pollSubConn = sub;
        }
      } else if (sub.u.tag.constiant == "clock") {
        if (sub.u.data.timeout < timeout) {
          timeout = sub.u.data.timeout;
          isClockPoll = true;
          clockSub = sub;
        }
      } else {
        console.log("poll_oneoff: unknown constiant " + sub.u.tag.constiant);
        return ERRNO_INVAL; // FIXME
      }
    }
    const events = [];
    if (isReadPollStdin || isReadPollConn || isClockPoll) {
      let readable = false;
      if (isReadPollStdin || (isClockPoll && timeout > 0)) {
        readable = pty.onWaitForReadable(timeout / 1000000000);
      }
      if (readable && isReadPollStdin) {
        const event = new Event();
        event.userdata = pollSubStdin.userdata;
        event.error = 0;
        event.type = new EventType("fd_read");
        events.push(event);
      }
      if (isReadPollConn) {
        const sockreadable = sockWaitForReadable();
        if (sockreadable == errStatus) {
          return ERRNO_INVAL;
        } else if (sockreadable == true) {
          const event = new Event();
          event.userdata = pollSubConn.userdata;
          event.error = 0;
          event.type = new EventType("fd_read");
          events.push(event);
        }
      }
      if (isClockPoll) {
        const event = new Event();
        event.userdata = clockSub.userdata;
        event.error = 0;
        event.type = new EventType("clock");
        events.push(event);
      }
    }
    const len = events.length;
    Event.write_bytes_array(buffer, out_ptr, events);
    buffer.setUint32(nevents_ptr, len, true);
    return 0;
  };
}
