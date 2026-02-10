/**
 * Setup script for impli web REPL
 * Checks requirements and registers service worker
 */

const DEBUG = true;

function log(...args) {
  if (DEBUG) {
    console.log("[DEBUG] Setup:", ...args);
  }
}

/**
 * Check if all required features are supported
 */
function checkRequirements() {
  const features = {
    webAssembly: typeof WebAssembly !== "undefined",
    sharedArrayBuffer: typeof SharedArrayBuffer !== "undefined",
    atomics: typeof Atomics !== "undefined",
    worker: typeof Worker !== "undefined",
    serviceWorker: "serviceWorker" in navigator,
    esModules: "noModule" in HTMLScriptElement.prototype,
  };

  const missing = Object.entries(features)
    .filter(([_, supported]) => !supported)
    .map(([name]) => name);

  return {
    supported: missing.length === 0,
    features,
    missing,
  };
}

/**
 * Register service worker for cross-origin isolation
 */
async function registerServiceWorker() {
  log("Registering service worker...");

  const registration = await navigator.serviceWorker.register("./sw.js", {
    scope: "./",
  });

  log("Service worker registered:", registration.scope);

  await navigator.serviceWorker.ready;

  // Check if we need to reload to activate
  if (!navigator.serviceWorker.controller) {
    log("Service worker installed for first time, reloading...");
    await new Promise((resolve) => setTimeout(resolve, 100));
    globalThis.location.reload();
    return new Promise(() => {}); // Prevent further execution
  }

  log("Service worker ready");
}

/**
 * Verify cross-origin isolation
 */
function verifyCrossOriginIsolation() {
  if (!crossOriginIsolated) {
    log("Warning: Not in cross-origin isolated context");

    // Try to create SharedArrayBuffer anyway
    try {
      new SharedArrayBuffer(1);
      log("SharedArrayBuffer available despite crossOriginIsolated=false");
      return true;
    } catch (e) {
      log("Error: SharedArrayBuffer not available:", e.message);
      return false;
    }
  }

  log("Cross-origin isolation active");
  return true;
}

/**
 * Load main application
 */
async function launch() {
  log("Launching application...");
  await import("./main.js");
  log("Launched application");
}

/**
 * Main initialization
 */
async function setup() {
  log("Starting setup...");

  // Check requirements
  const check = checkRequirements();

  if (!check.supported) {
    log("Missing features:", check.missing);
    globalThis.location.href = "./unsupported.html";
    return;
  }

  log("All features supported");

  try {
    // Register service worker
    await registerServiceWorker();

    // Verify cross-origin isolation
    if (!verifyCrossOriginIsolation()) {
      log("Cross-origin isolation check failed");
      globalThis.location.href = "./unsupported.html";
      return;
    }

    // Launch app
    await launch();
  } catch (error) {
    log("Launch failed:", error);
    globalThis.location.href = "./unsupported.html";
  }
}

// Start when DOM is ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", setup);
} else {
  setup();
}
