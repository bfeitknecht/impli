/**
 * Setup module for browser support checking and service worker registration
 */

/**
 * Check if the browser supports required features
 * @returns {boolean} True if all required features are supported
 */
function checkBrowserSupport() {
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

  return true;
}

/**
 * Register service worker to enable cross-origin isolation
 * @returns {Promise<boolean>} True if service worker is ready
 */
async function registerServiceWorker() {
  try {
    await navigator.serviceWorker.register("./sw.js", {
      scope: "./",
    });

    console.log("[INFO] Service Worker registered successfully");

    // If no controller yet, we need to reload to apply headers
    if (!navigator.serviceWorker.controller) {
      console.log("[INFO] Service Worker registered, reloading to apply headers");
      globalThis.location.reload();
      return false; // Will reload, so we won't continue
    }

    // Service worker is active and controlling the page
    return true;
  } catch (error) {
    console.error("[ERROR] Service Worker registration failed:", error);
    throw error;
  }
}

/**
 * Setup function - checks browser support and registers service worker
 * Redirects to unsupported.html if browser doesn't support required features
 * Reloads page if service worker was just registered
 */
export async function setup() {
  // Step 1: Check browser support
  if (!checkBrowserSupport()) {
    console.error("[ERROR] Browser does not support required features");
    globalThis.location.href = "./unsupported.html";
    throw new Error("Browser not supported");
  }

  // Step 2: Register service worker
  const ready = await registerServiceWorker();
  if (!ready) {
    // Page will reload, stop execution
    throw new Error("Reloading for service worker");
  }

  // Step 3: Verify cross-origin isolation is enabled
  if (!crossOriginIsolated) {
    console.error("[ERROR] Cross-Origin Isolation is not enabled");
    console.error("[ERROR] This should not happen if service worker is working correctly");
    globalThis.location.href = "./unsupported.html";
    throw new Error("Cross-origin isolation not enabled");
  }

  console.log("[INFO] Setup complete, browser support verified");
}
