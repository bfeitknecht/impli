/**
 * Build the web frontend into ./public: generate the examples module,
 * bundle page and worker, and mirror ./static. The WASM binary and its JSFFI
 * stub are placed into ./public by `nix run .#build` beforehand.
 */
const EXAMPLES = "../docs/examples";
const STATIC = "./static";
const OUTPUT = "./public";

async function generateExamples() {
  const examples: Record<string, string> = {};
  for await (const entry of Deno.readDir(EXAMPLES)) {
    if (!entry.isFile || !entry.name.endsWith(".imp")) continue;
    examples[entry.name.replace(/\.imp$/, "")] = await Deno.readTextFile(`${EXAMPLES}/${entry.name}`);
  }
  const names = Object.keys(examples).sort();
  const sorted = Object.fromEntries(names.map((name) => [name, examples[name]]));
  await Deno.writeTextFile(
    `${OUTPUT}/examples.js`,
    `// Generated from docs/examples by build.ts, do not edit\nexport const examples = ${JSON.stringify(sorted, null, 2)};\n`,
  );
  console.log(`✓ examples: ${names.join(", ")}`);
}

async function bundle(entrypoint: string, output: string) {
  // deno-lint-ignore no-explicit-any
  const result = await (Deno as any).bundle({
    entrypoints: [entrypoint],
    outputPath: `${OUTPUT}/${output}`,
    platform: "browser",
    format: "esm",
    minify: true,
  });
  if (!result.success) {
    console.error(`✗ bundling ${entrypoint} failed:`, result.errors);
    Deno.exit(1);
  }
  console.log(`✓ bundled ${entrypoint} -> ${output}`);
}

async function mirrorStatic(directory = STATIC, target = OUTPUT) {
  await Deno.mkdir(target, { recursive: true });
  for await (const entry of Deno.readDir(directory)) {
    const from = `${directory}/${entry.name}`;
    const to = `${target}/${entry.name}`;
    if (entry.isDirectory) await mirrorStatic(from, to);
    else await Deno.copyFile(from, to);
  }
  console.log(`✓ mirrored ${directory}`);
}

await Deno.mkdir(OUTPUT, { recursive: true });
await generateExamples();
await mirrorStatic();
await bundle("src/main.ts", "module.mjs");
await bundle("src/worker.ts", "worker.mjs");
