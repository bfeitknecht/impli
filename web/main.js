import { setup } from "./setup.js";
import { REPL } from "./REPL.js";

// Main entry point - setup infrastructure, then create and launch REPL
async function main() {
  try {
    // Setup service worker and verify WebWorker support
    await setup();

    // Create terminal instance
    const terminal = new Terminal({
      cursorBlink: true,
      fontFamily: '"CommitMono", "Courier New", monospace',
      fontSize: 13,
      theme: getThemeStyle(),
    });

    // Setup dynamic theme handling
    const applyTheme = () => terminal.setOption("theme", getThemeStyle());

    // Re-apply on system theme changes
    const mql = globalThis.matchMedia("(prefers-color-scheme: dark)");
    if ("addEventListener" in mql) {
      mql.addEventListener("change", applyTheme);
    } else if ("addListener" in mql) {
      mql.addListener(applyTheme);
    }

    // Observe :root style changes for manual CSS variable overrides
    const observer = new MutationObserver(applyTheme);
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["style", "class"],
    });

    // Create impli REPL addon (extends wasm-webterm)
    const repl = new REPL("./");

    // Expose to global scope for JSFFI
    globalThis.impli = repl;

    // Load the addon into the terminal
    terminal.loadAddon(repl);

    // Open terminal in the div
    terminal.open(document.getElementById("terminal"));

    // Focus the terminal
    terminal.focus();

    // WASM impli REPL will be automatically launched by the REPL.activate() method
    console.log("[INFO] Launching WASM impli REPL");
  } catch (error) {
    // Setup failed or triggered a reload - errors are already logged
    console.error("[ERROR] Failed to initialize:", error.message);
  }
}

// Wait for DOM to be ready, then start
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
