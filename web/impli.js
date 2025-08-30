export async function instantiate(url) {
  const instance = await fetch(url)
    .then((response) => response.arrayBuffer())
    .then((bytes) => WebAssembly.compile(bytes))
    .then((module) => WebAssembly.instantiate(module, stub(module)));

  const { initialize, execute, release } = instance.exports;
  const pointer = initialize();

  function interpret(input) {
    return execute(pointer, input);
  }
  release(pointer);

  return {
    interpret,
  };
}

function stub(module) {
  const object = {};
  for (const { module: mod, name } of WebAssembly.Module.imports(module)) {
    if (!object[mod]) object[mod] = {};
    object[mod][name] = (..._args) => 0;
  }
  return object;
}
