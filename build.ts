#!/usr/bin/env -S deno run --allow-read --allow-write

/**
 * Build script - generates examples.js from IMP files
 */

const EXAMPLES = "docs/examples";
const OUTPUT = "src/web/examples.js";

async function main() {
  console.log("Generating examples.js from IMP files...");

  const fs: Record<string, any> = {};

  for await (const entry of Deno.readDir(EXAMPLES)) {
    if (!entry.isFile || !entry.name.endsWith(".imp")) continue;

    const path = `/${entry.name}`;
    const content = await Deno.readTextFile(`${EXAMPLES}/${entry.name}`);

    fs[path] = {
      path,
      timestamps: {
        access: new Date(),
        change: new Date(),
        modification: new Date(),
      },
      mode: "string",
      content,
    };

    console.log(`  ✓ ${entry.name}`);
  }

  const output = `\
// DO NOT EDIT MANUALLY
// This file is auto-generated from ${EXAMPLES}

export const examples = ${JSON.stringify(fs, null, 2)};
`;

  await Deno.writeTextFile(OUTPUT, output);
  console.log(`✓ Generated ${OUTPUT} with ${Object.keys(fs).length} examples`);
}

if (import.meta.main) {
  await main();
}
