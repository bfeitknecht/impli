import { dedent } from "@/util.ts";

const EXAMPLES = "../docs/examples";
const OUTPUT = "./static";

async function generateExamples() {
  console.log("Generating examples.js from IMP files...");

  const entries: { [key: string]: any } = {};

  for await (const entry of Deno.readDir(EXAMPLES)) {
    if (!entry.isFile || !entry.name.endsWith(".imp")) continue;

    const path = `${entry.name}`;
    const content = await Deno.readTextFile(`${EXAMPLES}/${path}`);

    // Escape characters that would break the template literal in the generated file
    const escaped = content
      .replace(/\\/g, "\\\\")
      .replace(/`/g, "\\`")
      .replace(/\$/g, "\\$");

    // CHECK: Does this work?
    const now = new Date();
    const fd = {
      path: path,
      timestamps: {
        access: now,
        change: now,
        modification: now,
      },
      mode: "string",
      content: escaped.replace(/\s+/g, " "),
    };

    entries[path] = fd;
    console.log(`  ✓ ${entry.name}`);
  }
  const output = dedent`\
    // DO NOT EDIT MANUALLY
    // This file is auto-generated from docs/examples

    export const examples = ${JSON.stringify(entries)};`;

  await Deno.writeTextFile(OUTPUT + "/examples.js", output);
  console.log(`✓ Generated ${OUTPUT} with ${entries.length} examples`);
}

async function bundleApp() {
  // Bundling step using deno bundle
  console.log("Bundling application...");
  const result = await Deno.bundle({
    entrypoints: ["src/App.tsx", "src/sw.js"],
    outputDir: OUTPUT,
    platform: "browser",
    format: "esm",
    minify: true,
  });
  if (result.success) {
    console.log("✓ Successfully bundled");
  } else {
    console.error("✗ Bundling failed:");
    console.error(result.errors);
    Deno.exit(1);
  }
}

async function main() {
  await generateExamples();
  await bundleApp();
}

if (import.meta.main) {
  await main();
}
