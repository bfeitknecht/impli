export async function createImpli(wasmUrl) {
  // 1) Fetch the Wasm binary
  const resp = await fetch(wasmUrl);
  if (!resp.ok) {
    throw new Error(`Failed to fetch WASM at '${wasmUrl}': ${resp.statusText}`);
  }
  const bytes = await resp.arrayBuffer();

  // 2) Compile the module
  const module = await WebAssembly.compile(bytes);

  // 3) Auto-stub every imported function (including WASI/syscalls) to a no-op
  const importDescriptors = WebAssembly.Module.imports(module);
  const importObject = {};
  for (const { module: modName, name } of importDescriptors) {
    if (!importObject[modName]) importObject[modName] = {};
    importObject[modName][name] = (..._args) => 0;
  }

  // 4) Instantiate the module
  const instResult = await WebAssembly.instantiate(module, importObject);
  const instance = instResult.instance || instResult;
  if (!instance.exports) {
    throw new Error("WASM instantiation failed, no exports found");
  }
  console.log("[impli.js] Available exports:", Object.keys(instance.exports).join(", "));

  // 5) Extract the `impli` entrypoint and linear memory
  const { impli, memory } = instance.exports;
  if (typeof impli !== "function") {
    throw new Error("WASM export 'impli' not found or not a function");
  }
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error("WASM export 'memory' not found or not a Memory");
  }

  // 6) Set up a bump-pointer allocator in linear memory
  const HEAP = new Uint8Array(memory.buffer);
  let bump = 1024; // reserve 1K for runtime/metadata

  const encoder = new TextEncoder();
  const decoder = new TextDecoder();

  function writeString(str) {
    const data = encoder.encode(str + "\0");
    const ptr = bump;
    console.log("[impli.js] writeString: writing", data.length, "bytes at ptr", ptr);
    HEAP.set(data, ptr);
    bump += data.length;
    return ptr;
  }

  function readString(ptr) {
    let end = ptr;
    while (HEAP[end] !== 0) end++;
    const buf = HEAP.subarray(ptr, end);
    const str = decoder.decode(buf);
    console.log(
      "[impli.js] readString: read",
      buf.length,
      "bytes from ptr",
      ptr,
      "->",
      JSON.stringify(str),
    );
    return str;
  }

  // 7) Return a simple API for evaluating lines
  return {
    /**
     * Evaluate one line of IMP input and return its output (or error).
     */
    evaluate(line) {
      const inPtr = writeString(line);
      console.log("-> calling impli with ptr", inPtr);
      const outPtr = impli(inPtr);
      console.log("<- impli returned ptr", outPtr);
      return readString(outPtr);
    },

    /**
     * Expose the raw exports in case you need low-level access.
     */
    exports: instance.exports,
  };
}
