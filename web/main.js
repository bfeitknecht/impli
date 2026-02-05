import { Impli } from "./impli.js";

/**
 * Main entry point
 */
async function main() {
  try {
    // Create and start Impli instance, expose to global scope
    const impli = new Impli();
    await impli.start();
    globalThis.impli = impli;

    console.log("[INFO] impli started successfully");
  } catch (error) {
    console.error("[ERROR] Failed to initialize impli:", error);
  }
}

// Wait for DOM to be ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
