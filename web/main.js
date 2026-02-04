import { setup } from "./setup.js";
import { Impli } from "./impli.js";

/**
 * Main entry point
 */
async function main() {
  try {
    // Setup service worker and verify browser support
    await setup();

    // Create and start Impli instance
    const impli = new Impli();
    await impli.start();

    console.log("[INFO] impli started successfully");
  } catch (error) {
    console.error("[ERROR] Failed to initialize impli:", error);
  }
}

// Wait for DOM to be ready, then start
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
