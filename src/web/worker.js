/**
 * Web Worker for running impli WASM
 * Uses SharedArrayBuffer + Atomics for synchronous stdin
 */

import { WASI } from "@runno/wasi";
import stub from "./stub.js";
import { examples } from "./examples.js";
// State values for lock
const STATE_WAITING = 1;

const DEBUG = true;

function log(...args) {
  if (DEBUG) {
    console.log("[DEBUG] Worker:", ...args);
  }
}

// Message helpers
const msg = {
  stdout: (text) => ({ type: "stdout", text }),
  stderr: (text) => ({ type: "stderr", text }),
  stdin: (prompt) => ({ type: "stdin", prompt }),
};

let lock = null;
let buffer = null;

/**
 * Synchronous stdin that blocks until input is available
 */
function stdin() {
  log("stdin requested");

  // Get prompt from WASM exports
  const prompt = exports.getPrompt ? new TextDecoder().decode(exports.getPrompt()) : "> ";

  // Set state to waiting before requesting input to avoid race conditions
  Atomics.store(lock, 0, STATE_WAITING);

  // Request input from main thread
  self.postMessage(msg.stdin(prompt));

  // Block until main thread provides input
  log("Blocking for input...");
  const result = Atomics.wait(lock, 0, STATE_WAITING);
  log("Unblocked, result:", result);

  // Read input data from buffer
  const inputBytes = new Uint8Array(buffer);
  const text = new TextDecoder().decode(inputBytes);
  log("Input received:", text);

  // Return as Uint8Array for WASI
  return new TextEncoder().encode(text);
}

/**
 * Handle messages from main thread
 */
self.onmessage = async (event) => {
  lock = new Int32Array(event.data.lock.buffer);
  buffer = event.data.buffer;

  log("Starting WASM...");
  await startWASM(event.data.wasmURL);
};

let exports = {};

/**
 * Start the WASM module
 */
async function startWASM(wasmURL) {
  try {
    log("Creating WASI instance...");

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

    log("Fetching WASM binary...");
    const wasmResponse = await fetch(wasmURL);
    const wasmBytes = await wasmResponse.arrayBuffer();

    log("Instantiating WASM...");
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

    log("Starting WASM execution...");
    wasm.instance.exports.start();

    log("WASM execution completed");
  } catch (error) {
    log("Error:", error);
    throw error;
  }
}

log("Worker initialized");
