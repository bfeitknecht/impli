import { dedent } from "@/util.ts";

const EXAMPLES = "../docs/examples";
const OUTPUT = "./public";

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

/** Download-and-extract a file from an npm registry tarball (cached). */
async function fromNpmTarPackage(
  pkg: string,
  version: string,
  member: string,
  target: string,
) {
  try {
    await Deno.stat(target);
    return; // already extracted
  } catch {
    // fall through and fetch
  }
  const url =
    `https://registry.npmjs.org/${pkg}/-/${pkg.split("/").pop()}-${version}.tgz`;
  console.log(`Fetching ${url} for ${member}...`);
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`failed to fetch ${url}: ${response.status}`);
  }
  const temp = `${OUTPUT}/.${pkg.split("/").pop()}.tgz`;
  await Deno.writeFile(temp, new Uint8Array(await response.arrayBuffer()));
  const extract = new Deno.Command("tar", {
    args: ["xzf", temp, "-O", `package/${member}`],
  });
  const { code, stdout, stderr } = await extract.output();
  if (code !== 0) {
    throw new Error(`tar failed: ${new TextDecoder().decode(stderr)}`);
  }
  await Deno.mkdir(`${OUTPUT}/typst`, { recursive: true });
  await Deno.writeFile(target, stdout);
  await Deno.remove(temp);
  console.log(`  ✓ ${target}`);
}

/** Copy the vendored curryst proof-tree package into the site root. */
async function copyCurryst() {
  await Deno.copyFile("vendor/curryst/curryst.typ", `${OUTPUT}/curryst.typ`);
  console.log("✓ vendored curryst");
}

/** Provide the Typst compiler and renderer WASM at runtime URLs. */
async function typstAssets() {
  const version = "0.7.0";
  await fromNpmTarPackage(
    "@myriaddreamin/typst-ts-web-compiler",
    version,
    "pkg/typst_ts_web_compiler_bg.wasm",
    `${OUTPUT}/typst/typst_ts_web_compiler_bg.wasm`,
  );
  await fromNpmTarPackage(
    "@myriaddreamin/typst-ts-renderer",
    version,
    "pkg/typst_ts_renderer_bg.wasm",
    `${OUTPUT}/typst/typst_ts_renderer_bg.wasm`,
  );
}

/** Short content hash used for cache-busting query strings. */
async function hash(path: string): Promise<string> {
  const bytes = await Deno.readFile(path);
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return [...new Uint8Array(digest)]
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("")
    .slice(0, 12);
}

/**
 * Rewrite the copied index.html so stylesheet and script references carry a
 * content hash; Pages serves them without revalidation headers otherwise and
 * stale caches were observed to render a mismatched UI.
 */
async function cacheBustIndex() {
  const index = `${OUTPUT}/index.html`;
  const css = await hash(`${OUTPUT}/styles.css`);
  const js = await hash(`${OUTPUT}/module.mjs`);
  const original = await Deno.readTextFile(index);
  const rewritten = original
    .replace('"./styles.css"', `"./styles.css?v=${css}"`)
    .replace('"./module.mjs"', `"./module.mjs?v=${js}"`);
  if (rewritten === original) {
    console.warn("⚠ index.html references not found; cache busting skipped");
  }
  await Deno.writeTextFile(index, rewritten);
  console.log(`✓ cache-busted index.html (css ${css}, js ${js})`);
}

/** Mirror static assets into the output root (also done by the nix build). */
async function copyStatic() {
  for await (const entry of Deno.readDir("static")) {
    if (!entry.isFile) continue;
    await Deno.copyFile(`static/${entry.name}`, `${OUTPUT}/${entry.name}`);
  }
}

async function main() {
  await Deno.mkdir(OUTPUT, { recursive: true });
  await copyStatic();
  await generateExamples();
  await bundleApp();
  await copyCurryst();
  await typstAssets();
  await cacheBustIndex();
}

if (import.meta.main) {
  await main();
}
