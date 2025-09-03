import { create } from "./impli.js";

const terminal = document.getElementById("terminal");
const input = document.getElementById("input");
function display(text) {
  terminal.textContent += text + "\n";
  terminal.scrollTop = terminal.scrollHeight;
}

async function main() {
  display("Hello, World!");
  const impli = await create("./impli.wasm");
  console.log(await impli.hello());
}

main();
