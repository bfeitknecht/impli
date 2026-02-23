import { dedent } from "@/util.ts";

const EXAMPLES = "../docs/examples";
const OUTPUT = "./static";

async function generateExamples() {
  console.log("Generating examples.js from IMP files...");

  const entries: { [key: string]: any } = {};

  for await (const entry of Deno.readDir(EXAMPLES)) {
    if (!entry.isFile || !entry.name.endsWith(".imp")) continue;

    const file = `${entry.name}`;
    const content = await Deno.readTextFile(`${EXAMPLES}/${file}`);

    // Escape critical characters and clean up comments
    const clean = content
      .replace(/\\/g, "\\\\")
      .replace(/`/g, "\\`")
      .replace(/\$/g, "\\$")
      .replace(/\/\/.*$/gm, "")
      .replace(/(\/\*.*\*\/)/gm, "")
      .replace(/\s+/g, " ")
      .trim();

    const path = "/" + file;
    const fd = {
      path: path,
      timestamps: {
        access: "__NOW__",
        change: "__NOW__",
        modification: "__NOW__",
      },
      mode: "string",
      content: clean,
    };

    entries[path] = fd;
    console.log(`  ✓ ${entry.name}`);
  }
  const output = dedent`\
    // DO NOT EDIT MANUALLY
    // This file is auto-generated from docs/examples

    const now = new Date();
    export const examples = ${
    JSON.stringify(entries).replace(/"__NOW__"/g, "now") // Sorry for the spaghetti
  };`;

  const result = OUTPUT + "/examples.js";
  await Deno.writeTextFile(result, output);
  console.log(
    `✓ Generated ${result} with ${Object.keys(entries).length} examples`,
  );
}

async function bundleApp() {
  console.log("Bundling application...");
  const result = await Deno.bundle({
    entrypoints: ["src/App.tsx"],
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
