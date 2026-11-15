/**
 * Headless smoke test of the WASM build: drives the exports like the worker
 * does and checks output of examples, input, big integers and interrupts.
 * Run `deno task smoke` after `nix run .#build`.
 */
import { WASI } from "@runno/wasi";
import stub from "stub";
import { examples } from "examples";

const output: string[] = [];
let answers: string[] = [];
let interruptAfter = -1;

(globalThis as any).impli = {
  output: (text: string) => output.push(text),
  clear: () => {},
  input: (_: string) => Promise.resolve(answers.shift() ?? "\x04"),
  pause: async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
    return interruptAfter >= 0 && --interruptAfter < 0 ? 1 : 0;
  },
  exists: (path: string) => (path.replace(/\.imp$/, "") in examples ? 1 : 0),
  load: (path: string) => (examples as Record<string, string>)[path.replace(/\.imp$/, "")],
  save: () => {},
};

const wasi = new WASI({ args: ["impli"], env: {}, stdin: () => null });
const exports: any = {};
const { instance } = await WebAssembly.instantiate(await Deno.readFile("public/impli.wasm"), {
  ...wasi.getImportObject(),
  ghc_wasm_jsffi: stub(exports),
});
Object.assign(exports, instance.exports);
wasi.initialize({ instance } as never, { ghc_wasm_jsffi: stub(exports) } as never);
await exports.boot();

let failures = 0;
async function check(name: string, lines: string[], expected: string[] | ((out: string[]) => boolean)) {
  output.length = 0;
  const start = performance.now();
  for (const line of lines) await exports.submit(line);
  const elapsed = (performance.now() - start).toFixed(0);
  const ok = typeof expected === "function" ? expected(output) : JSON.stringify(output) === JSON.stringify(expected);
  if (!ok) failures++;
  console.log(`${ok ? "✓" : "✗"} ${name} (${elapsed} ms)${ok ? "" : `\n    got: ${JSON.stringify(output)}`}`);
}

await check("arithmetic", ["print 6 * 7"], ["42"]);
await check("gauss example", [":reset", ":load gauss.imp"], (out) => out.includes("4950"));
await check("factorial example", [":load factorial"], (out) => out.includes("2432902008176640000"));
await check("big integers", ["x := 2; repeat 100 times x *= 2 end; print x"], [(2n ** 101n).toString()]);
answers = ["41"];
await check("read input", ["read x; print x + 1"], ["42"]);
if ((await exports.incomplete("while x < 3 do")) !== 1 || (await exports.incomplete("skip")) !== 0) {
  failures++;
  console.log("✗ incomplete");
} else console.log("✓ incomplete");
await check("loop performance", ["i := 0; s := 0; while i < 1000000 do s += i; i += 1 end; print s"], ["499999500000"]);
interruptAfter = 3;
await check("interrupt", ["while true do skip end"], ["*** ERROR: interrupted"]);
interruptAfter = -1;
await check("session survives interrupt", ["print s"], ["499999500000"]);
await check("break", ["x := 0; while true do break; x := 1 end; print x"], ["0"]);
await check("abbreviated load", [":reset", ":l gauss.imp"], (out) => out.includes("4950"));
await check(
  "profile verbosity",
  [":set semantics structural", ":set verbose profile", "print 1", ":set verbose normal", ":set semantics natural"],
  (out) => out.some((o) => /^\[profile\] elapsed: [\d.]+ ms, \d+ steps$/.test(o)),
);
await exports.submit(":set verbose debug");
const debugPrompt = await exports.prompt();
await exports.submit(":set verbose normal");
if (debugPrompt !== "IMP* " || (await exports.prompt()) !== "IMP> ") {
  failures++;
  console.log(`✗ prompt\n    got: ${JSON.stringify(debugPrompt)}`);
} else console.log("✓ prompt");
for (const name of Object.keys(examples)) {
  answers = ["5", "3"];
  await check(`example ${name}`, [":reset", `:load ${name}`], (out) => !out.some((o) => o.startsWith("*** ERROR")));
}

console.log(failures === 0 ? "\nall passed" : `\n${failures} failed`);
Deno.exit(failures === 0 ? 0 : 1);
