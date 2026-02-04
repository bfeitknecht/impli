// Setup module for service worker and WebWorker support checks
// Handles only the infrastructure setup, not REPL/Terminal instantiation

/**
 * Setup service worker and verify WebWorker support
 * This is a void function that either completes successfully or throws/reloads
 */
export async function setup() {
  // Step 1: Check if WebWorker support is available
  if (!checkWebWorkerSupport()) {
    displayWebWorkerError();
    throw new Error("WebWorker support not available");
  }

  // Step 2: Register service worker for cross-origin isolation
  const serviceWorker = await registerServiceWorker();

  // Step 3: If service worker was just registered, reload to apply headers
  if (serviceWorker && !navigator.serviceWorker.controller) {
    console.log("[INFO] Service Worker registered, reloading page to apply headers");
    globalThis.location.reload();
    throw new Error("Reloading to apply service worker"); // Stop execution
  }

  console.log("[INFO] Setup complete, environment ready");
}

/**
 * Check if WebWorker and SharedArrayBuffer are supported
 * @returns {boolean} True if all required features are supported
 */
function checkWebWorkerSupport() {
  // Check for SharedArrayBuffer support
  if (typeof SharedArrayBuffer === "undefined") {
    console.error("[ERROR] SharedArrayBuffer is not supported");
    return false;
  }

  // Check for Worker support
  if (typeof Worker === "undefined") {
    console.error("[ERROR] WebWorker is not supported");
    return false;
  }

  // Check for ServiceWorker support
  if (!("serviceWorker" in navigator)) {
    console.error("[ERROR] ServiceWorker is not supported");
    return false;
  }

  // Check if cross-origin isolation is needed but not available
  // (This check happens after service worker registration and page reload)
  if (navigator.serviceWorker.controller && !crossOriginIsolated) {
    console.error("[ERROR] Cross-Origin Isolation is not enabled");
    return false;
  }

  return true;
}

/**
 * Display error message when WebWorker support is not available
 */
function displayWebWorkerError() {
  const terminalDiv = document.getElementById("terminal");
  terminalDiv.innerHTML = `
    <div style="padding: 2rem; max-width: 600px; margin: 0 auto; font-family: system-ui, -apple-system, sans-serif;">
      <h1 style="color: #d32f2f; margin-bottom: 1rem;">WebWorker Support Required</h1>
      <p style="margin-bottom: 1rem; line-height: 1.6;">
        This application requires WebWorker and SharedArrayBuffer support to function properly.
      </p>
      <p style="margin-bottom: 1rem; line-height: 1.6;">
        Your browser or environment does not support these features, which are necessary for running
        the WebAssembly-based REPL.
      </p>
      <h2 style="color: #616161; font-size: 1.1rem; margin-top: 2rem; margin-bottom: 0.5rem;">Requirements:</h2>
      <ul style="line-height: 1.8; margin-bottom: 1rem;">
        <li>Modern browser (Chrome 68+, Firefox 79+, Safari 15.2+, Edge 79+)</li>
        <li>Secure context (HTTPS or localhost)</li>
        <li>Cross-Origin Isolation enabled (handled by service worker)</li>
      </ul>
      <h2 style="color: #616161; font-size: 1.1rem; margin-top: 2rem; margin-bottom: 0.5rem;">Troubleshooting:</h2>
      <ul style="line-height: 1.8;">
        <li>Ensure you're using a modern browser version</li>
        <li>Check that the page is served over HTTPS or from localhost</li>
        <li>Try refreshing the page after the service worker is registered</li>
        <li>Check the browser console for specific error messages</li>
      </ul>
    </div>
  `;
}

/**
 * Register service worker to enable cross-origin isolation
 * @returns {Promise<ServiceWorker|null>} Active service worker or null
 */
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
      console.error("[ERROR] Service Worker registration failed:", error);
      return null;
    }
  } else {
    console.warn("[WARN] Service Workers not supported in this browser");
    return null;
  }
}
