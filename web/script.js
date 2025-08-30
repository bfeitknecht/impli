import { instantiate } from "./impli.js";

const terminal = document.getElementById("terminal");
const input = document.getElementById("input");
function display(text) {
  terminal.textContent += text;
  terminal.scrollTop = terminal.scrollHeight;
}

async function main() {
  const { interpret } = await instantiate("impli.wasm");
  display("Hello, World!");

  input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") {
      const command = input.value;
      display("> " + command);

      const result = JSON.parse(interpret(command));
      if (result.exception) {
        display("Error: " + result.exception);
      } else if (result.output) {
        display(result.output);
      }
      input.value = "";
    }
  });
}

main();
