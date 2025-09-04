import { IMPLI } from "./impli.js";

const terminal = document.getElementById("terminal");
const input = document.getElementById("input");
function display(text) {
  terminal.textContent += text + "\n";
  terminal.scrollTop = terminal.scrollHeight;
}

async function main() {
  display("Hello, World!");
  const impli = await new IMPLI().initialize();

  input.addEventListener("keypress", async (e) => {
    if (e.key === "Enter") {
      const command = input.value;
      display("> " + command);
      input.value = "";
      await impli.interpret(command);
    }
  });
}

main();
