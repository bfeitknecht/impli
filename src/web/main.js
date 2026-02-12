import { Impli } from "@/impli.js";

/**
 * Main entry point
 */
async function main() {
  try {
    // Create and expose impli instance globally
    const impli = new Impli();
    globalThis.impli = impli;

    // Start impli
    await impli.start();
    console.log("[DEBUG] Main: impli started successfully");
  } catch (error) {
    console.error("[DEBUG] Main: Failed to initialize impli:", error);
  }
}

// Wait for DOM to be ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
