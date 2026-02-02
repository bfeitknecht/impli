// Main script for impli WASM web interface
// Uses wasm-webterm to run impli WASM binary in the browser

// Impli subclass of WasmWebTerm that auto-launches impli REPL
class Impli extends WasmWebTerm.default {
  printWelcomeMessagePlusControlSequences() {
    const dedent = (strings, ...values) => {
      let raw = "";
      for (let i = 0; i < strings.length; i++) {
        raw += strings[i];
        if (i < values.length) raw += values[i];
      }
      const lines = raw.split("\n");
      const normalized = lines.map((line) => line.replace(/^[ \t]+/, ""));
      return normalized.join("\r\n") + "\r\n";
    };

    // https://patorjk.com/software/taag/#p=display&f=Broadway+KB&t=impli
    const logo = dedent`\x1bc\x1b[1m
      __ ___  __ _____ __    __
      || || \\/ | ||_// ||    ||
      || ||    | ||    ||__| ||
      \x1b[0m`;

    const message = dedent`\
      Execute IMP statements in the browser and inspect the resulting state.
      Made with <3 by Basil Feitknecht`;

    return logo + message;
  }

  // Disable WasmWebTerm REPL
  repl() {}

  // Start impli REPL
  async activate(xterm) {
    const backup = console.log;
    // Surpress annoying log messages
    console.log = () => {};
    try {
      await super.activate(xterm); // Set up addons, registers JS commands
      await this.runWasmCommand("impli", []); // Run WASM impli REPL
    } catch (err) {
      console.log = backup;
      console.log(err);
    } finally {
      console.log = backup;
    }
  }
}

// Initialize everything when WebWorker is ready
async function init() {
  // Register service worker first
  const serviceWorker = await registerServiceWorker();

  // If service worker was just registered, reload the page to apply headers
  if (serviceWorker && !navigator.serviceWorker.controller) {
    console.log("[INFO] Service Worker registered, reloading page to apply headers");
    globalThis.location.reload();
    return;
  }

  // Create terminal instance
  const terminal = new Terminal({
    cursorBlink: true,
    fontFamily: '"CommitMono", "Courier New", monospace',
    fontSize: 13,
    theme: getThemeStyle(),
  });

  // Apply theme from CSS
  const applyThemeStyle = () => terminal.setOption("theme", getThemeStyle());

  // Re-apply on system theme changes (CSS media query updates variables)
  const mql = globalThis.matchMedia("(prefers-color-scheme: dark)");
  if ("addEventListener" in mql) {
    mql.addEventListener("change", applyThemeStyle);
  } else if ("addListener" in mql) {
    mql.addListener(applyThemeStyle);
  }

  // Observe :root style changes to catch manual CSS variable overrides
  const observer = new MutationObserver(applyThemeStyle);
  observer.observe(document.documentElement, {
    attributes: true,
    attributeFilter: ["style", "class"],
  });

  // Create Impli addon (extends wasm-webterm)
  // The first parameter is the path to predelivered binaries
  const impli = new Impli("./");

  // Load the addon into the terminal
  terminal.loadAddon(impli);

  // Open terminal in the div
  terminal.open(document.getElementById("terminal"));

  // Focus the terminal
  terminal.focus();

  // WASM impli REPL will be automatically launched by the Impli.activate() method
  console.log("[INFO] Launching WASM impli REPL");
}

// Get xterm theme from CSS
function getThemeStyle() {
  const getStyleVar = (name) =>
    getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  return {
    background: getStyleVar("--terminal-bg") || undefined,
    foreground: getStyleVar("--terminal-fg") || undefined,
    cursor: getStyleVar("--terminal-cursor") || undefined,
    selectionBackground: getStyleVar("--terminal-selection") || undefined,
  };
}

// Register ServiceWorker to enable WebWorker
async function registerServiceWorker() {
  // Register service worker to enable Cross-Origin Isolation
  // Required for SharedArrayBuffer and WebWorkers support in wasm-webterm
  if ("serviceWorker" in navigator) {
    try {
      const registration = await navigator.serviceWorker.register("./sw.js", {
        scope: "./",
      });

      console.log("[INFO] Service Worker registered successfully:", registration);

      // Wait for the service worker to be active
      if (registration.active) {
        return registration.active;
      }

      // Wait for the service worker to activate
      return new Promise((resolve) => {
        const serviceWorker = registration.installing || registration.waiting;
        if (serviceWorker) {
          serviceWorker.addEventListener("statechange", (e) => {
            if (e.target.state === "activated") {
              resolve(e.target);
            }
          });
        } else {
          resolve(registration.active);
        }
      });
    } catch (error) {
      console.error("Service Worker registration failed:", error);
      return null;
    }
  } else {
    console.warn("Service Workers not supported in this browser");
    return null;
  }
}

// Wait for DOM to be ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", init);
} else {
  init();
}
