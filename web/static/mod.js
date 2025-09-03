// ../../../../../Library/Caches/deno/npm/registry.npmjs.org/@bjorn3/browser_wasi_shim/0.4.2/dist/wasi_defs.js
var CLOCKID_REALTIME = 0;
var CLOCKID_MONOTONIC = 1;
var ERRNO_SUCCESS = 0;
var ERRNO_BADF = 8;
var ERRNO_INVAL = 28;
var ERRNO_NAMETOOLONG = 37;
var ERRNO_NOSYS = 52;
var ERRNO_NOTSUP = 58;
var RIGHTS_FD_DATASYNC = 1 << 0;
var RIGHTS_FD_READ = 1 << 1;
var RIGHTS_FD_SEEK = 1 << 2;
var RIGHTS_FD_FDSTAT_SET_FLAGS = 1 << 3;
var RIGHTS_FD_SYNC = 1 << 4;
var RIGHTS_FD_TELL = 1 << 5;
var RIGHTS_FD_WRITE = 1 << 6;
var RIGHTS_FD_ADVISE = 1 << 7;
var RIGHTS_FD_ALLOCATE = 1 << 8;
var RIGHTS_PATH_CREATE_DIRECTORY = 1 << 9;
var RIGHTS_PATH_CREATE_FILE = 1 << 10;
var RIGHTS_PATH_LINK_SOURCE = 1 << 11;
var RIGHTS_PATH_LINK_TARGET = 1 << 12;
var RIGHTS_PATH_OPEN = 1 << 13;
var RIGHTS_FD_READDIR = 1 << 14;
var RIGHTS_PATH_READLINK = 1 << 15;
var RIGHTS_PATH_RENAME_SOURCE = 1 << 16;
var RIGHTS_PATH_RENAME_TARGET = 1 << 17;
var RIGHTS_PATH_FILESTAT_GET = 1 << 18;
var RIGHTS_PATH_FILESTAT_SET_SIZE = 1 << 19;
var RIGHTS_PATH_FILESTAT_SET_TIMES = 1 << 20;
var RIGHTS_FD_FILESTAT_GET = 1 << 21;
var RIGHTS_FD_FILESTAT_SET_SIZE = 1 << 22;
var RIGHTS_FD_FILESTAT_SET_TIMES = 1 << 23;
var RIGHTS_PATH_SYMLINK = 1 << 24;
var RIGHTS_PATH_REMOVE_DIRECTORY = 1 << 25;
var RIGHTS_PATH_UNLINK_FILE = 1 << 26;
var RIGHTS_POLL_FD_READWRITE = 1 << 27;
var RIGHTS_SOCK_SHUTDOWN = 1 << 28;
var Iovec = class _Iovec {
  static read_bytes(view, ptr) {
    const iovec = new _Iovec();
    iovec.buf = view.getUint32(ptr, true);
    iovec.buf_len = view.getUint32(ptr + 4, true);
    return iovec;
  }
  static read_bytes_array(view, ptr, len) {
    const iovecs = [];
    for (let i = 0; i < len; i++) {
      iovecs.push(_Iovec.read_bytes(view, ptr + 8 * i));
    }
    return iovecs;
  }
};
var Ciovec = class _Ciovec {
  static read_bytes(view, ptr) {
    const iovec = new _Ciovec();
    iovec.buf = view.getUint32(ptr, true);
    iovec.buf_len = view.getUint32(ptr + 4, true);
    return iovec;
  }
  static read_bytes_array(view, ptr, len) {
    const iovecs = [];
    for (let i = 0; i < len; i++) {
      iovecs.push(_Ciovec.read_bytes(view, ptr + 8 * i));
    }
    return iovecs;
  }
};
var FDFLAGS_APPEND = 1 << 0;
var FDFLAGS_DSYNC = 1 << 1;
var FDFLAGS_NONBLOCK = 1 << 2;
var FDFLAGS_RSYNC = 1 << 3;
var FDFLAGS_SYNC = 1 << 4;
var FSTFLAGS_ATIM = 1 << 0;
var FSTFLAGS_ATIM_NOW = 1 << 1;
var FSTFLAGS_MTIM = 1 << 2;
var FSTFLAGS_MTIM_NOW = 1 << 3;
var OFLAGS_CREAT = 1 << 0;
var OFLAGS_DIRECTORY = 1 << 1;
var OFLAGS_EXCL = 1 << 2;
var OFLAGS_TRUNC = 1 << 3;
var EVENTTYPE_CLOCK = 0;
var EVENTRWFLAGS_FD_READWRITE_HANGUP = 1 << 0;
var SUBCLOCKFLAGS_SUBSCRIPTION_CLOCK_ABSTIME = 1 << 0;
var Subscription = class _Subscription {
  static read_bytes(view, ptr) {
    return new _Subscription(view.getBigUint64(ptr, true), view.getUint8(ptr + 8), view.getUint32(ptr + 16, true), view.getBigUint64(ptr + 24, true), view.getUint16(ptr + 36, true));
  }
  constructor(userdata, eventtype, clockid, timeout, flags) {
    this.userdata = userdata;
    this.eventtype = eventtype;
    this.clockid = clockid;
    this.timeout = timeout;
    this.flags = flags;
  }
};
var Event = class {
  write_bytes(view, ptr) {
    view.setBigUint64(ptr, this.userdata, true);
    view.setUint16(ptr + 8, this.error, true);
    view.setUint8(ptr + 10, this.eventtype);
  }
  constructor(userdata, error, eventtype) {
    this.userdata = userdata;
    this.error = error;
    this.eventtype = eventtype;
  }
};
var RIFLAGS_RECV_PEEK = 1 << 0;
var RIFLAGS_RECV_WAITALL = 1 << 1;
var ROFLAGS_RECV_DATA_TRUNCATED = 1 << 0;
var SDFLAGS_RD = 1 << 0;
var SDFLAGS_WR = 1 << 1;

// ../../../../../Library/Caches/deno/npm/registry.npmjs.org/@bjorn3/browser_wasi_shim/0.4.2/dist/debug.js
var Debug = class Debug2 {
  enable(enabled) {
    this.log = createLogger(enabled === void 0 ? true : enabled, this.prefix);
  }
  get enabled() {
    return this.isEnabled;
  }
  constructor(isEnabled) {
    this.isEnabled = isEnabled;
    this.prefix = "wasi:";
    this.enable(isEnabled);
  }
};
function createLogger(enabled, prefix) {
  if (enabled) {
    const a = console.log.bind(console, "%c%s", "color: #265BA0", prefix);
    return a;
  } else {
    return () => {
    };
  }
}
var debug = new Debug(false);

// ../../../../../Library/Caches/deno/npm/registry.npmjs.org/@bjorn3/browser_wasi_shim/0.4.2/dist/wasi.js
var WASIProcExit = class extends Error {
  constructor(code) {
    super("exit with exit code " + code);
    this.code = code;
  }
};
var WASI = class WASI2 {
  start(instance) {
    this.inst = instance;
    try {
      instance.exports._start();
      return 0;
    } catch (e) {
      if (e instanceof WASIProcExit) {
        return e.code;
      } else {
        throw e;
      }
    }
  }
  initialize(instance) {
    this.inst = instance;
    if (instance.exports._initialize) {
      instance.exports._initialize();
    }
  }
  constructor(args, env, fds, options = {}) {
    this.args = [];
    this.env = [];
    this.fds = [];
    debug.enable(options.debug);
    this.args = args;
    this.env = env;
    this.fds = fds;
    const self = this;
    this.wasiImport = {
      args_sizes_get(argc, argv_buf_size) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        buffer.setUint32(argc, self.args.length, true);
        let buf_size = 0;
        for (const arg of self.args) {
          buf_size += arg.length + 1;
        }
        buffer.setUint32(argv_buf_size, buf_size, true);
        debug.log(buffer.getUint32(argc, true), buffer.getUint32(argv_buf_size, true));
        return 0;
      },
      args_get(argv, argv_buf) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        const orig_argv_buf = argv_buf;
        for (let i = 0; i < self.args.length; i++) {
          buffer.setUint32(argv, argv_buf, true);
          argv += 4;
          const arg = new TextEncoder().encode(self.args[i]);
          buffer8.set(arg, argv_buf);
          buffer.setUint8(argv_buf + arg.length, 0);
          argv_buf += arg.length + 1;
        }
        if (debug.enabled) {
          debug.log(new TextDecoder("utf-8").decode(buffer8.slice(orig_argv_buf, argv_buf)));
        }
        return 0;
      },
      environ_sizes_get(environ_count, environ_size) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        buffer.setUint32(environ_count, self.env.length, true);
        let buf_size = 0;
        for (const environ of self.env) {
          buf_size += new TextEncoder().encode(environ).length + 1;
        }
        buffer.setUint32(environ_size, buf_size, true);
        debug.log(buffer.getUint32(environ_count, true), buffer.getUint32(environ_size, true));
        return 0;
      },
      environ_get(environ, environ_buf) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        const orig_environ_buf = environ_buf;
        for (let i = 0; i < self.env.length; i++) {
          buffer.setUint32(environ, environ_buf, true);
          environ += 4;
          const e = new TextEncoder().encode(self.env[i]);
          buffer8.set(e, environ_buf);
          buffer.setUint8(environ_buf + e.length, 0);
          environ_buf += e.length + 1;
        }
        if (debug.enabled) {
          debug.log(new TextDecoder("utf-8").decode(buffer8.slice(orig_environ_buf, environ_buf)));
        }
        return 0;
      },
      clock_res_get(id, res_ptr) {
        let resolutionValue;
        switch (id) {
          case CLOCKID_MONOTONIC: {
            resolutionValue = 5000n;
            break;
          }
          case CLOCKID_REALTIME: {
            resolutionValue = 1000000n;
            break;
          }
          default:
            return ERRNO_NOSYS;
        }
        const view = new DataView(self.inst.exports.memory.buffer);
        view.setBigUint64(res_ptr, resolutionValue, true);
        return ERRNO_SUCCESS;
      },
      clock_time_get(id, precision, time) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (id === CLOCKID_REALTIME) {
          buffer.setBigUint64(time, BigInt((/* @__PURE__ */ new Date()).getTime()) * 1000000n, true);
        } else if (id == CLOCKID_MONOTONIC) {
          let monotonic_time;
          try {
            monotonic_time = BigInt(Math.round(performance.now() * 1e6));
          } catch (e) {
            monotonic_time = 0n;
          }
          buffer.setBigUint64(time, monotonic_time, true);
        } else {
          buffer.setBigUint64(time, 0n, true);
        }
        return 0;
      },
      fd_advise(fd, offset, len, advice) {
        if (self.fds[fd] != void 0) {
          return ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_allocate(fd, offset, len) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_allocate(offset, len);
        } else {
          return ERRNO_BADF;
        }
      },
      fd_close(fd) {
        if (self.fds[fd] != void 0) {
          const ret = self.fds[fd].fd_close();
          self.fds[fd] = void 0;
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_datasync(fd) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_sync();
        } else {
          return ERRNO_BADF;
        }
      },
      fd_fdstat_get(fd, fdstat_ptr) {
        if (self.fds[fd] != void 0) {
          const { ret, fdstat } = self.fds[fd].fd_fdstat_get();
          if (fdstat != null) {
            fdstat.write_bytes(new DataView(self.inst.exports.memory.buffer), fdstat_ptr);
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_fdstat_set_flags(fd, flags) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_fdstat_set_flags(flags);
        } else {
          return ERRNO_BADF;
        }
      },
      fd_fdstat_set_rights(fd, fs_rights_base, fs_rights_inheriting) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_fdstat_set_rights(fs_rights_base, fs_rights_inheriting);
        } else {
          return ERRNO_BADF;
        }
      },
      fd_filestat_get(fd, filestat_ptr) {
        if (self.fds[fd] != void 0) {
          const { ret, filestat } = self.fds[fd].fd_filestat_get();
          if (filestat != null) {
            filestat.write_bytes(new DataView(self.inst.exports.memory.buffer), filestat_ptr);
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_filestat_set_size(fd, size) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_filestat_set_size(size);
        } else {
          return ERRNO_BADF;
        }
      },
      fd_filestat_set_times(fd, atim, mtim, fst_flags) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_filestat_set_times(atim, mtim, fst_flags);
        } else {
          return ERRNO_BADF;
        }
      },
      fd_pread(fd, iovs_ptr, iovs_len, offset, nread_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const iovecs = Iovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
          let nread = 0;
          for (const iovec of iovecs) {
            const { ret, data } = self.fds[fd].fd_pread(iovec.buf_len, offset);
            if (ret != ERRNO_SUCCESS) {
              buffer.setUint32(nread_ptr, nread, true);
              return ret;
            }
            buffer8.set(data, iovec.buf);
            nread += data.length;
            offset += BigInt(data.length);
            if (data.length != iovec.buf_len) {
              break;
            }
          }
          buffer.setUint32(nread_ptr, nread, true);
          return ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_prestat_get(fd, buf_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const { ret, prestat } = self.fds[fd].fd_prestat_get();
          if (prestat != null) {
            prestat.write_bytes(buffer, buf_ptr);
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_prestat_dir_name(fd, path_ptr, path_len) {
        if (self.fds[fd] != void 0) {
          const { ret, prestat } = self.fds[fd].fd_prestat_get();
          if (prestat == null) {
            return ret;
          }
          const prestat_dir_name = prestat.inner.pr_name;
          const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
          buffer8.set(prestat_dir_name.slice(0, path_len), path_ptr);
          return prestat_dir_name.byteLength > path_len ? ERRNO_NAMETOOLONG : ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_pwrite(fd, iovs_ptr, iovs_len, offset, nwritten_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const iovecs = Ciovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
          let nwritten = 0;
          for (const iovec of iovecs) {
            const data = buffer8.slice(iovec.buf, iovec.buf + iovec.buf_len);
            const { ret, nwritten: nwritten_part } = self.fds[fd].fd_pwrite(data, offset);
            if (ret != ERRNO_SUCCESS) {
              buffer.setUint32(nwritten_ptr, nwritten, true);
              return ret;
            }
            nwritten += nwritten_part;
            offset += BigInt(nwritten_part);
            if (nwritten_part != data.byteLength) {
              break;
            }
          }
          buffer.setUint32(nwritten_ptr, nwritten, true);
          return ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_read(fd, iovs_ptr, iovs_len, nread_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const iovecs = Iovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
          let nread = 0;
          for (const iovec of iovecs) {
            const { ret, data } = self.fds[fd].fd_read(iovec.buf_len);
            if (ret != ERRNO_SUCCESS) {
              buffer.setUint32(nread_ptr, nread, true);
              return ret;
            }
            buffer8.set(data, iovec.buf);
            nread += data.length;
            if (data.length != iovec.buf_len) {
              break;
            }
          }
          buffer.setUint32(nread_ptr, nread, true);
          return ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_readdir(fd, buf, buf_len, cookie, bufused_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          let bufused = 0;
          while (true) {
            const { ret, dirent } = self.fds[fd].fd_readdir_single(cookie);
            if (ret != 0) {
              buffer.setUint32(bufused_ptr, bufused, true);
              return ret;
            }
            if (dirent == null) {
              break;
            }
            if (buf_len - bufused < dirent.head_length()) {
              bufused = buf_len;
              break;
            }
            const head_bytes = new ArrayBuffer(dirent.head_length());
            dirent.write_head_bytes(new DataView(head_bytes), 0);
            buffer8.set(new Uint8Array(head_bytes).slice(0, Math.min(head_bytes.byteLength, buf_len - bufused)), buf);
            buf += dirent.head_length();
            bufused += dirent.head_length();
            if (buf_len - bufused < dirent.name_length()) {
              bufused = buf_len;
              break;
            }
            dirent.write_name_bytes(buffer8, buf, buf_len - bufused);
            buf += dirent.name_length();
            bufused += dirent.name_length();
            cookie = dirent.d_next;
          }
          buffer.setUint32(bufused_ptr, bufused, true);
          return 0;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_renumber(fd, to) {
        if (self.fds[fd] != void 0 && self.fds[to] != void 0) {
          const ret = self.fds[to].fd_close();
          if (ret != 0) {
            return ret;
          }
          self.fds[to] = self.fds[fd];
          self.fds[fd] = void 0;
          return 0;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_seek(fd, offset, whence, offset_out_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const { ret, offset: offset_out } = self.fds[fd].fd_seek(offset, whence);
          buffer.setBigInt64(offset_out_ptr, offset_out, true);
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_sync(fd) {
        if (self.fds[fd] != void 0) {
          return self.fds[fd].fd_sync();
        } else {
          return ERRNO_BADF;
        }
      },
      fd_tell(fd, offset_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const { ret, offset } = self.fds[fd].fd_tell();
          buffer.setBigUint64(offset_ptr, offset, true);
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      fd_write(fd, iovs_ptr, iovs_len, nwritten_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const iovecs = Ciovec.read_bytes_array(buffer, iovs_ptr, iovs_len);
          let nwritten = 0;
          for (const iovec of iovecs) {
            const data = buffer8.slice(iovec.buf, iovec.buf + iovec.buf_len);
            const { ret, nwritten: nwritten_part } = self.fds[fd].fd_write(data);
            if (ret != ERRNO_SUCCESS) {
              buffer.setUint32(nwritten_ptr, nwritten, true);
              return ret;
            }
            nwritten += nwritten_part;
            if (nwritten_part != data.byteLength) {
              break;
            }
          }
          buffer.setUint32(nwritten_ptr, nwritten, true);
          return ERRNO_SUCCESS;
        } else {
          return ERRNO_BADF;
        }
      },
      path_create_directory(fd, path_ptr, path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          return self.fds[fd].path_create_directory(path);
        } else {
          return ERRNO_BADF;
        }
      },
      path_filestat_get(fd, flags, path_ptr, path_len, filestat_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          const { ret, filestat } = self.fds[fd].path_filestat_get(flags, path);
          if (filestat != null) {
            filestat.write_bytes(buffer, filestat_ptr);
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      path_filestat_set_times(fd, flags, path_ptr, path_len, atim, mtim, fst_flags) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          return self.fds[fd].path_filestat_set_times(flags, path, atim, mtim, fst_flags);
        } else {
          return ERRNO_BADF;
        }
      },
      path_link(old_fd, old_flags, old_path_ptr, old_path_len, new_fd, new_path_ptr, new_path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[old_fd] != void 0 && self.fds[new_fd] != void 0) {
          const old_path = new TextDecoder("utf-8").decode(buffer8.slice(old_path_ptr, old_path_ptr + old_path_len));
          const new_path = new TextDecoder("utf-8").decode(buffer8.slice(new_path_ptr, new_path_ptr + new_path_len));
          const { ret, inode_obj } = self.fds[old_fd].path_lookup(old_path, old_flags);
          if (inode_obj == null) {
            return ret;
          }
          return self.fds[new_fd].path_link(new_path, inode_obj, false);
        } else {
          return ERRNO_BADF;
        }
      },
      path_open(fd, dirflags, path_ptr, path_len, oflags, fs_rights_base, fs_rights_inheriting, fd_flags, opened_fd_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          debug.log(path);
          const { ret, fd_obj } = self.fds[fd].path_open(dirflags, path, oflags, fs_rights_base, fs_rights_inheriting, fd_flags);
          if (ret != 0) {
            return ret;
          }
          self.fds.push(fd_obj);
          const opened_fd = self.fds.length - 1;
          buffer.setUint32(opened_fd_ptr, opened_fd, true);
          return 0;
        } else {
          return ERRNO_BADF;
        }
      },
      path_readlink(fd, path_ptr, path_len, buf_ptr, buf_len, nread_ptr) {
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          debug.log(path);
          const { ret, data } = self.fds[fd].path_readlink(path);
          if (data != null) {
            const data_buf = new TextEncoder().encode(data);
            if (data_buf.length > buf_len) {
              buffer.setUint32(nread_ptr, 0, true);
              return ERRNO_BADF;
            }
            buffer8.set(data_buf, buf_ptr);
            buffer.setUint32(nread_ptr, data_buf.length, true);
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      path_remove_directory(fd, path_ptr, path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          return self.fds[fd].path_remove_directory(path);
        } else {
          return ERRNO_BADF;
        }
      },
      path_rename(fd, old_path_ptr, old_path_len, new_fd, new_path_ptr, new_path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0 && self.fds[new_fd] != void 0) {
          const old_path = new TextDecoder("utf-8").decode(buffer8.slice(old_path_ptr, old_path_ptr + old_path_len));
          const new_path = new TextDecoder("utf-8").decode(buffer8.slice(new_path_ptr, new_path_ptr + new_path_len));
          let { ret, inode_obj } = self.fds[fd].path_unlink(old_path);
          if (inode_obj == null) {
            return ret;
          }
          ret = self.fds[new_fd].path_link(new_path, inode_obj, true);
          if (ret != ERRNO_SUCCESS) {
            if (self.fds[fd].path_link(old_path, inode_obj, true) != ERRNO_SUCCESS) {
              throw "path_link should always return success when relinking an inode back to the original place";
            }
          }
          return ret;
        } else {
          return ERRNO_BADF;
        }
      },
      path_symlink(old_path_ptr, old_path_len, fd, new_path_ptr, new_path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const old_path = new TextDecoder("utf-8").decode(buffer8.slice(old_path_ptr, old_path_ptr + old_path_len));
          const new_path = new TextDecoder("utf-8").decode(buffer8.slice(new_path_ptr, new_path_ptr + new_path_len));
          return ERRNO_NOTSUP;
        } else {
          return ERRNO_BADF;
        }
      },
      path_unlink_file(fd, path_ptr, path_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer);
        if (self.fds[fd] != void 0) {
          const path = new TextDecoder("utf-8").decode(buffer8.slice(path_ptr, path_ptr + path_len));
          return self.fds[fd].path_unlink_file(path);
        } else {
          return ERRNO_BADF;
        }
      },
      poll_oneoff(in_ptr, out_ptr, nsubscriptions) {
        if (nsubscriptions === 0) {
          return ERRNO_INVAL;
        }
        if (nsubscriptions > 1) {
          debug.log("poll_oneoff: only a single subscription is supported");
          return ERRNO_NOTSUP;
        }
        const buffer = new DataView(self.inst.exports.memory.buffer);
        const s = Subscription.read_bytes(buffer, in_ptr);
        const eventtype = s.eventtype;
        const clockid = s.clockid;
        const timeout = s.timeout;
        if (eventtype !== EVENTTYPE_CLOCK) {
          debug.log("poll_oneoff: only clock subscriptions are supported");
          return ERRNO_NOTSUP;
        }
        let getNow = void 0;
        if (clockid === CLOCKID_MONOTONIC) {
          getNow = () => BigInt(Math.round(performance.now() * 1e6));
        } else if (clockid === CLOCKID_REALTIME) {
          getNow = () => BigInt((/* @__PURE__ */ new Date()).getTime()) * 1000000n;
        } else {
          return ERRNO_INVAL;
        }
        const endTime = (s.flags & SUBCLOCKFLAGS_SUBSCRIPTION_CLOCK_ABSTIME) !== 0 ? timeout : getNow() + timeout;
        while (endTime > getNow()) {
        }
        const event = new Event(s.userdata, ERRNO_SUCCESS, eventtype);
        event.write_bytes(buffer, out_ptr);
        return ERRNO_SUCCESS;
      },
      proc_exit(exit_code) {
        throw new WASIProcExit(exit_code);
      },
      proc_raise(sig) {
        throw "raised signal " + sig;
      },
      sched_yield() {
      },
      random_get(buf, buf_len) {
        const buffer8 = new Uint8Array(self.inst.exports.memory.buffer).subarray(buf, buf + buf_len);
        if ("crypto" in globalThis && (typeof SharedArrayBuffer === "undefined" || !(self.inst.exports.memory.buffer instanceof SharedArrayBuffer))) {
          for (let i = 0; i < buf_len; i += 65536) {
            crypto.getRandomValues(buffer8.subarray(i, i + 65536));
          }
        } else {
          for (let i = 0; i < buf_len; i++) {
            buffer8[i] = Math.random() * 256 | 0;
          }
        }
      },
      sock_recv(fd, ri_data, ri_flags) {
        throw "sockets not supported";
      },
      sock_send(fd, si_data, si_flags) {
        throw "sockets not supported";
      },
      sock_shutdown(fd, how) {
        throw "sockets not supported";
      },
      sock_accept(fd, flags) {
        throw "sockets not supported";
      }
    };
  }
};

// ../../../../../Library/Caches/deno/npm/registry.npmjs.org/@bjorn3/browser_wasi_shim/0.4.2/dist/fd.js
var Inode = class _Inode {
  static issue_ino() {
    return _Inode.next_ino++;
  }
  static root_ino() {
    return 0n;
  }
  constructor() {
    this.ino = _Inode.issue_ino();
  }
};
Inode.next_ino = 1n;

// web/src/ghc_wasm_jsffi.js
var JSValManager = class {
  #lastk = 0;
  #kv = /* @__PURE__ */ new Map();
  newJSVal(v) {
    const k = ++this.#lastk;
    this.#kv.set(k, v);
    return k;
  }
  // A separate has() call to ensure we can store undefined as a value
  // too. Also, unconditionally check this since the check is cheap
  // anyway, if the check fails then there's a use-after-free to be
  // fixed.
  getJSVal(k) {
    if (!this.#kv.has(k)) {
      throw new WebAssembly.RuntimeError(`getJSVal(${k})`);
    }
    return this.#kv.get(k);
  }
  // Check for double free as well.
  freeJSVal(k) {
    if (!this.#kv.delete(k)) {
      throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
    }
  }
};
var setImmediate = await (async () => {
  if (globalThis.setImmediate) {
    return globalThis.setImmediate;
  }
  if (globalThis.Deno) {
    try {
      return (await import("node:timers")).setImmediate;
    } catch {
    }
  }
  if (globalThis.scheduler) {
    return (cb, ...args) => scheduler.postTask(() => cb(...args));
  }
  if (globalThis.MessageChannel) {
    class SetImmediate {
      #fs = [];
      #mc = new MessageChannel();
      constructor() {
        this.#mc.port1.addEventListener("message", () => {
          this.#fs.pop()();
        });
        this.#mc.port1.start();
      }
      setImmediate(cb, ...args) {
        this.#fs.push(() => cb(...args));
        this.#mc.port2.postMessage(void 0);
      }
    }
    const sm = new SetImmediate();
    return (cb, ...args) => sm.setImmediate(cb, ...args);
  }
  return (cb, ...args) => setTimeout(cb, 0, ...args);
})();
var ghc_wasm_jsffi_default = (__exports) => {
  const __ghc_wasm_jsffi_jsval_manager = new JSValManager();
  const __ghc_wasm_jsffi_finalization_registry = globalThis.FinalizationRegistry ? new FinalizationRegistry((sp) => __exports.rts_freeStablePtr(sp)) : {
    register: () => {
    },
    unregister: () => true
  };
  return {
    newJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),
    getJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),
    freeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),
    scheduleWork: () => setImmediate(__exports.rts_schedulerLoop),
    ZC4ZCimplizm3zi0zi0zi0zminplacezmimplizmwasmZCMainZC: ($1) => console.log($1),
    ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => $1.reject(new WebAssembly.RuntimeError($2)),
    ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => $1.resolve($2),
    ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => $1.resolve($2),
    ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => $1.resolve(),
    ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => {
      $1.throwTo = () => {
      };
    },
    ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1, $2) => {
      $1.throwTo = (err) => __exports.rts_promiseThrowTo($2, err);
    },
    ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: () => {
      let res, rej;
      const p = new Promise((resolve, reject) => {
        res = resolve;
        rej = reject;
      });
      p.resolve = res;
      p.reject = rej;
      return p;
    },
    ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => `${$1.stack ? $1.stack : $1}`,
    ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1, $2) => new TextDecoder("utf-8", {
      fatal: true
    }).decode(new Uint8Array(__exports.memory.buffer, $1, $2)),
    ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1, $2, $3) => new TextEncoder().encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written,
    ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => $1.length,
    ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => {
      try {
        __ghc_wasm_jsffi_finalization_registry.unregister($1);
      } catch {
      }
    },
    ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1, $2) => $1.then(() => __exports.rts_promiseResolveUnit($2), (err) => __exports.rts_promiseReject($2, err)),
    ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC: async ($1) => new Promise((res) => setTimeout(res, $1 / 1e3))
  };
};

// web/src/impli.js
var IMPLI = class {
  constructor() {
  }
  async initialize() {
    this.exports = {};
    const wasi = new WASI([], [], []);
    const wasm = await WebAssembly.instantiateStreaming(fetch("./impli.wasm"), {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: ghc_wasm_jsffi_default(this.exports)
    });
    wasi.initialize(wasm.instance);
    Object.assign(this.exports, wasm.instance.exports);
    this.pointer = this.exports.initialize();
    return this;
  }
  async interpret(input2) {
    await this.exports.execute(this.pointer, input2);
  }
};

// web/src/index.js
var terminal = document.getElementById("terminal");
var input = document.getElementById("input");
function display(text) {
  terminal.textContent += text + "\n";
  terminal.scrollTop = terminal.scrollHeight;
}
async function main() {
  display("Hello, World!");
  const impli = await new IMPLI().initialize();
  console.log(await impli.exports.hello());
}
main();
