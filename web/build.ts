/**
 * Build script - generates examples.js from IMP files
 */

const EXAMPLES = "../docs/examples";
const OUTPUT = "./src/static/examples.js";

async function main() {
  console.log("Generating examples.js from IMP files...");

  const entries: string[] = [];

  for await (const entry of Deno.readDir(EXAMPLES)) {
    if (!entry.isFile || !entry.name.endsWith(".imp")) continue;

    const path = `/${entry.name}`;
    const content = await Deno.readTextFile(`${EXAMPLES}${path}`);

    // Escape characters that would break the template literal in the generated file
    const escaped = content.replace(/\\/g, "\\\\").replace(/`/g, "\\`").replace(
      /\$/g,
      "\\$",
    );

    entries.push(`\
  "${path}": {
    path: "${path}",
    timestamps: {
      access: new Date(),
      change: new Date(),
      modification: new Date(),
    },
    mode: string,
    content: \`${escaped}\`,
    }`);

    console.log(`  ✓ ${entry.name}`);
  }

  const output = `\
// DO NOT EDIT MANUALLY
// This file is auto-generated from docs/examples

export const examples = {
${entries.join(",\n")}
};`;

  await Deno.writeTextFile(OUTPUT, output);
  console.log(`✓ Generated ${OUTPUT} with ${entries.length} examples`);

  // Bundling step using deno bundle
  console.log("Bundling application with deno bundle...");
  const result = await Deno.bundle({
    entrypoints: ["src/App.tsx"],
    outputPath: "src/module.mjs",
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

if (import.meta.main) {
  await main();
}
