const OUTPUT = "./public";
const EXAMPLES = "../docs/examples";

interface Rule {
  name: string;
  raw: string;
  comment: string | null;
}

interface Example {
  path: string;
  mode: string;
  content: string;
  timestamps: { access: Date; change: Date; modification: Date };
}

async function generateExamplesJSON() {
  console.log("Generating examples.json from IMP files...");
  const fs: Record<string, Example> = {};

  for await (const f of Deno.readDir(EXAMPLES)) {
    if (!f.isFile || !f.name.endsWith(".imp")) continue;

    const path = "/" + f.name;
    const content = (await Deno.readTextFile(`${EXAMPLES}/${f.name}`))
      .replace(/\\/g, "\\\\")
      .replace(/`/g, "\\`")
      .replace(/\$/g, "\\$")
      .replace(/\/\/.*$/gm, "")
      .replace(/(\/\*.*\*\/)/gm, "")
      .replace(/\s+/g, " ")
      .trim();

    const now = new Date();
    const timestamp = {
      access: now,
      change: now,
      modification: now,
    };

    fs[path] = {
      path: path,
      mode: "string",
      content: content,
      timestamps: timestamp,
    };

    console.log(`  ✓ ${f.name}`);
  }

  const examples = JSON.stringify(fs);
  const output = OUTPUT + "/examples.json";
  await Deno.writeTextFile(output, examples);
  console.log(
    `✓ Generated ${output} with ${Object.keys(examples).length} examples`,
  );
}

async function parseEBNFGrammar(): Promise<Rule[]> {
  console.log("Parsing EBNF grammar...");
  const grammarPath = "../docs/IMP.ebnf";
  const content = await Deno.readTextFile(grammarPath);

  const rules: Rule[] = [];
  const lines = content.split("\n");

  let i = 0;
  while (i < lines.length) {
    const line = lines[i].trim();
    i++;

    if (!line) continue;

    const ruleMatch = line.match(/^([A-Z][a-zA-Z0-9]*)\s*::=\s*(.+)/);
    if (!ruleMatch) continue;

    const ruleName = ruleMatch[1];
    let ruleBody = ruleMatch[2];
    let comment: string | null = null;

    const commentMatch = ruleBody.match(/(.+?)\s*\/\*\s*(.+?)\s*\*\/\s*$/);
    if (commentMatch) {
      ruleBody = commentMatch[1].trim();
      comment = commentMatch[2].trim();
    }

    while (i < lines.length) {
      const nextLine = lines[i].trim();
      if (!nextLine) {
        i++;
        continue;
      }
      if (!nextLine.startsWith("|")) break;

      i++;
      let nextBody = nextLine.substring(1).trim();

      const nextComment = nextBody.match(/(.+?)\s*\/\*\s*(.+?)\s*\*\/\s*$/);
      if (nextComment) {
        nextBody = nextComment[1].trim();
        if (!comment) comment = nextComment[2].trim();
      }

      ruleBody += " | " + nextBody;
    }

    rules.push({ name: ruleName, raw: ruleBody, comment });
  }

  console.log(`  ✓ Parsed ${rules.length} rules`);
  return rules;
}

async function generateEBNFJson() {
  console.log("Generating ebnf.json...");
  const ebnfData = await parseEBNFGrammar();
  const output = OUTPUT + "/ebnf.json";
  await Deno.writeTextFile(output, JSON.stringify(ebnfData, null, 2));
  console.log(`✓ Generated ${output}`);
}

async function bundleApp() {
  console.log("Bundling application...");
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
    const p = new Deno.Command("cp", {
      args: ["-r", source + "/", destination + "/"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stderr } = await p.output();

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

async function main() {
  await Deno.mkdir(OUTPUT, { recursive: true });
  await generateExamplesJSON();
  await generateEBNFJson();
  await bundleApp();
  await copyStaticAssets();
}

if (import.meta.main) {
  await main();
}
