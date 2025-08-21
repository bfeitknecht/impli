// script.js (browser)
import { createImpli } from "./impli.js";

(async () => {
  const term = document.getElementById("terminal");
  const box = document.getElementById("input");
  const append = (txt) => {
    term.textContent += txt + "\n";
    term.scrollTop = term.scrollHeight;
  };

  append("Loading the IMP language interpreter...");
  let repl;
  // expose to console for debugging
  window.repl = null;
  try {
    repl = await createImpli("impli.wasm");
    // bind for console access
    window.repl = repl;
  } catch (e) {
    append("Load error: " + e);
    return;
  }
  append("Done!");

  box.focus();

  // On Enter, send the line into the REPL and display the result
  box.addEventListener("keydown", (ev) => {
    if (ev.key !== "Enter") return;
    ev.preventDefault();
    const line = box.value;
    box.value = "";
    append("> " + line);
    // Schedule evaluation to avoid blocking the UI
    const promise = new Promise((res) => {
      setTimeout(() => res("TIMED OUT"), 1000);
      try {
        const r = repl.evaluate(line);
        res(r);
      } catch (e) {
        res("ERR:" + e);
      }
    });
    promise.then((result) => append(result));
  });
})();
