/// <reference lib="webworker" />

/**
 * Web Worker for running impli WASM
 * Uses SharedArrayBuffer + Atomics for synchronous stdin
 */

import { WASI } from "@runno/wasi";
import stub from "@/static/stub.js";
import { examples } from "@/static/examples.js";
import { STATE_WAITING, log } from "./util.ts";

declare const self: DedicatedWorkerGlobalScope;

// Message helpers
const msg = {
  stdout: (text: string) => ({ type: "stdout", text }),
  stderr: (text: string) => ({ type: "stderr", text }),
  stdin: (prompt: string) => ({ type: "stdin", prompt }),
};

let lock: Int32Array | null = null;
let buffer: SharedArrayBuffer | null = null;
let exports: any = {};

/**
 * Synchronous stdin that blocks until input is available
 */
function stdin(): Uint8Array {
  log("Worker", "stdin requested");

  if (!lock || !buffer) {
    throw new Error("Worker not initialized with shared buffers");
  }

  // Get prompt from WASM exports
  const prompt = exports.getPrompt ? new TextDecoder().decode(exports.getPrompt()) : "> ";

  // Set state to waiting before requesting input to avoid race conditions
  Atomics.store(lock, 0, STATE_WAITING);

  // Request input from main thread
  self.postMessage(msg.stdin(prompt));

  // Block until main thread provides input
  log("Worker", "Blocking for input...");
  Atomics.wait(lock, 0, STATE_WAITING);
  log("Worker", "Unblocked");

  // Read input data from buffer
  const inputBytes = new Uint8Array(buffer);
  const text = new TextDecoder().decode(inputBytes).replace(/\0/g, "");
  log("Worker", "Input received:", text);

  // Return as Uint8Array for WASI
  return new TextEncoder().encode(text);
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
        const text = typeof data === "string" ? data : new TextDecoder().decode(data);
        self.postMessage(msg.stdout(text));
      },
      stderr: (data) => {
        const text = typeof data === "string" ? data : new TextDecoder().decode(data);
        self.postMessage(msg.stderr(text));
      },
    });

    log("Worker", "Fetching WASM binary from:", wasmURL);
    const wasmResponse = await fetch(wasmURL);
    const wasmBytes = await wasmResponse.arrayBuffer();

    log("Worker", "Instantiating WASM...");
    const wasm = await WebAssembly.instantiate(wasmBytes, {
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
    if (typeof (wasm.instance.exports as any).start === "function") {
      (wasm.instance.exports as any).start();
    }

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
