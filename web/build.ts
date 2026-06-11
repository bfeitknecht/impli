// Dedent template literal
// deno-lint-ignore no-explicit-any
function dedent(strings: TemplateStringsArray, ...values: any[]) {
  const raw = strings.reduce(
    (acc, str, i) => {
      let val = String(values[i] ?? "");
      if (val.includes("\n")) {
        const match = (acc + str).match(/(?:^|\n)( *)$/);
        if (match) {
          val = val.split("\n").map((l, j) => j === 0 ? l : match[1] + l).join(
            "\n",
          );
        }
      }
      return acc + str + val;
    },
    "",
  );

  const lines = raw.split("\n");

  const indent = lines
    .filter((line) => line.trim().length > 0)
    .reduce((min, line) => {
      const match = line.match(/^(\s*)/);
      return match ? Math.min(min, match[1].length) : min;
    }, Infinity);

  if (indent === Infinity) return raw;

  return lines
    .map((line) => line.slice(indent))
    .join("\n");
}

const EXAMPLES = "../docs/examples";
const OUTPUT = "./public";

async function generateExamples() {
  console.log("Generating examples.js from IMP files...");

  // deno-lint-ignore no-explicit-any
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
    JSON.stringify(entries).replace(/"__NOW__"/g, "now") // Sorry for the spaghetti...
  };`;

  const result = OUTPUT + "/examples.js";
  await Deno.writeTextFile(result, output);
  console.log(
    `✓ Generated ${result} with ${Object.keys(entries).length} examples`,
  );
}

async function bundleApp() {
  console.log("Bundling application...");
  // LSP is old and forgets `.bundle()` was reintroduced to Deno
  // deno-lint-ignore no-explicit-any
  const result = await (Deno as any).bundle({
    entrypoints: ["src/App.ts"],
    outputPath: `${OUTPUT}/module.mjs`,
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

async function copyStaticAssets() {
  console.log("Copying static assets...");
  const source = "./static";
  const destination = OUTPUT;

  try {
    // Use an external command for recursive copying
    const p = new Deno.Command("cp", {
      args: ["-r", source + "/", destination + "/"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stdout, stderr } = await p.output();

    if (code === 0) {
      console.log(`✓ Successfully copied static assets to ${destination}`);
    } else {
      const decoder = new TextDecoder();
      console.error("✗ Failed to copy static assets:");
      console.error(decoder.decode(stderr));
      Deno.exit(1);
    }
  } catch (error) {
    console.error("✗ An error occurred during asset copying:", error);
    Deno.exit(1);
  }
}

async function copyGrammarFile() {
  console.log("Copying EBNF grammar file...");
  const source = "../docs/IMP.ebnf";
  const destination = `${OUTPUT}/IMP.ebnf`;

  try {
    const content = await Deno.readTextFile(source);
    await Deno.writeTextFile(destination, content);
    console.log(`✓ Successfully copied grammar to ${destination}`);
  } catch (error) {
    console.error("✗ Failed to copy grammar file:", error);
    Deno.exit(1);
  }
}

async function main() {
  await Deno.mkdir(OUTPUT, { recursive: true });
  await generateExamples();
  await bundleApp();
  await copyStaticAssets();
  await copyGrammarFile();
}

if (import.meta.main) {
  await main();
}
