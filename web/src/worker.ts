/// <reference lib="webworker" />

/**
 * Web Worker for running impli WASM
 * Uses SharedArrayBuffer + Atomics for synchronous stdin
 */

import { WASI } from "@runno/wasi";
import stub from "@/static/stub.js";
import { examples } from "@/static/examples.js";
import { log, WAITING } from "@/util.ts";

declare const self: DedicatedWorkerGlobalScope;

// Message helpers
const msg = {
  stdout: (text: string) => ({ type: "stdout", text }),
  stderr: (text: string) => ({ type: "stderr", text }),
  stdin: (prompt: string) => ({ type: "stdin", prompt }),
};

let lock: Int32Array;
let buffer: SharedArrayBuffer;
let exports: any = {};

/**
 * Synchronous stdin that blocks until input is available
 */
function stdin(): string {
  log("Worker", "stdin requested");

  if (!lock || !buffer) {
    throw new Error("Worker not initialized with shared buffers");
  }

  // Get prompt from WASM exports
  const prompt = exports.getPrompt();

  // Set state to waiting before requesting input to avoid race conditions
  Atomics.store(lock, 0, WAITING);

  // Request input from main thread
  self.postMessage(msg.stdin(prompt));

  // Block until main thread provides input
  log("Worker", "Blocking for input...");
  Atomics.wait(lock, 0, WAITING);
  log("Worker", "Unblocked");

  // Read input data from buffer
  const text = new TextDecoder().decode(new Uint8Array(buffer));
  log("Worker", "Input received:", text);

  return text;
}

/**
 * Start the WASM module
 */
async function startWASM(wasmURL: string) {
  try {
    log("Worker", "Creating WASI instance...");

    const wasi = new WASI({
      args: ["impli"],
      env: {},
      fs: examples,
      stdin: stdin,
      stdout: (data) => {
        const text = typeof data === "string"
          ? data
          : new TextDecoder().decode(data);
        self.postMessage(msg.stdout(text));
      },
      stderr: (data) => {
        const text = typeof data === "string"
          ? data
          : new TextDecoder().decode(data);
        self.postMessage(msg.stderr(text));
      },
    });

    log("Worker", "Instantiating WASM streaming from:", wasmURL);
    const wasm = await WebAssembly.instantiateStreaming(fetch(wasmURL), {
      ...wasi.getImportObject(),
      ghc_wasm_jsffi: stub(exports),
    });

    // Knot-tying
    Object.assign(exports, wasm.instance.exports);

    // Initialize WASI
    wasi.initialize(wasm, {
      ghc_wasm_jsffi: stub(exports),
    });

    log("Worker", "Starting WASM execution...");
    exports.start();

    log("Worker", "WASM execution completed");
  } catch (error) {
    log("Worker", "Error:", error);
    throw error;
  }
}

/**
 * Handle messages from main thread
 */
self.onmessage = async (event: MessageEvent) => {
  const { lock: l, buffer: b, wasmURL } = event.data;
  lock = new Int32Array(l.buffer);
  buffer = b;

  log("Worker", "Starting WASM...");
  await startWASM(wasmURL);
};

log("Worker", "Worker initialized");
