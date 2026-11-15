/**
 * Web worker hosting the interpreter. The WASM instance keeps the session in
 * Haskell memory, so a reload always starts fresh. Long-running
 * interpretation yields to this worker's event loop regularly through
 * `pause`, which is where a cancel request is noticed.
 */
import { WASI } from "@runno/wasi";
import stub from "stub";
import { examples } from "examples";
import type { Request, Response } from "@/protocol.ts";

interface Exports {
  boot(): Promise<void>;
  submit(line: string): Promise<number>;
  complete(prefix: string): Promise<string>;
  incomplete(input: string): Promise<number>;
}

const post = (message: Response) => (self as unknown as Worker).postMessage(message);

/** Example file name, accepting optional leading slash and extension. */
function example(path: string): string | undefined {
  const name = path.replace(/^\/+/, "").replace(/\.imp$/, "");
  return (examples as Record<string, string>)[name];
}

let interrupted = false;
let answer: ((value: string) => void) | null = null;

// INFO: A message channel yields a macrotask without the clamping of setTimeout
const channel = new MessageChannel();
const waiting: (() => void)[] = [];
channel.port1.onmessage = () => waiting.shift()?.();
const macrotask = () =>
  new Promise<void>((resolve) => {
    waiting.push(resolve);
    channel.port2.postMessage(null);
  });

(globalThis as unknown as { impli: unknown }).impli = {
  output: (text: string) => post({ type: "output", text }),
  clear: () => post({ type: "clear" }),
  input: (prompt: string) =>
    new Promise<string>((resolve) => {
      if (interrupted) return resolve("\x03");
      answer = resolve;
      post({ type: "ask", prompt });
    }),
  pause: async () => {
    await macrotask();
    return interrupted ? 1 : 0;
  },
  exists: (path: string) => (example(path) === undefined ? 0 : 1),
  load: (path: string) => example(path) ?? "",
  save: (path: string, content: string) => post({ type: "save", path, content }),
};

async function start(): Promise<Exports> {
  const wasi = new WASI({ args: ["impli"], env: {}, stdin: () => null });
  const exports: Partial<Exports> = {};
  const { instance } = await WebAssembly.instantiateStreaming(fetch("./impli.wasm"), {
    ...wasi.getImportObject(),
    ghc_wasm_jsffi: stub(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize({ instance } as never, { ghc_wasm_jsffi: stub(exports) } as never);
  return exports as Exports;
}

const ready = start();
let queue = Promise.resolve();

self.onmessage = (event: MessageEvent<Request>) => {
  const message = event.data;
  switch (message.type) {
    case "cancel":
      interrupted = true;
      if (answer) {
        answer("\x03");
        answer = null;
      }
      return;
    case "answer":
      answer?.(message.value);
      answer = null;
      return;
    case "submit":
      // INFO: Submissions run one after another
      queue = queue.then(async () => {
        const exports = await ready;
        interrupted = false;
        await exports.submit(message.line);
        interrupted = false;
        post({ type: "done" });
      });
      return;
    case "incomplete":
      ready.then(async (exports) =>
        post({ type: "reply", id: message.id, value: (await exports.incomplete(message.input)) !== 0 })
      );
      return;
    case "complete":
      ready.then(async (exports) => {
        const candidates = (await exports.complete(message.prefix)).split("\n").filter((c) => c);
        post({ type: "reply", id: message.id, value: candidates });
      });
      return;
  }
};

ready.then(async (exports) => {
  await exports.boot();
  post({ type: "ready" });
}).catch((error) => post({ type: "failed", message: String(error) }));
