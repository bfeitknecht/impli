/// <reference lib="webworker" />

/**
 * Service Worker for impli web REPL
 *
 * This service worker adds the necessary headers to enable SharedArrayBuffer
 * by setting up cross-origin isolation:
 * - Cross-Origin-Embedder-Policy: require-corp
 * - Cross-Origin-Opener-Policy: same-origin
 *
 * This is necessary because GitHub Pages doesn't allow custom HTTP headers,
 * but these headers are required for SharedArrayBuffer and Atomics to work.
 */

import { log } from "@/util.ts";

declare const self: ServiceWorkerGlobalScope;

// Install event - skip waiting to activate immediately
self.addEventListener("install", (_event) => {
  log("SW", "Installing...");
  self.skipWaiting();
});

// Activate event - claim clients immediately
self.addEventListener("activate", (event) => {
  log("SW", "Activating...");
  event.waitUntil(
    self.clients.claim().then(() => {
      log("SW", "Service worker activated and claimed clients");
    }),
  );
});

// Fetch event - add required headers for cross-origin isolation
self.addEventListener("fetch", (event) => {
  event.respondWith(
    fetch(event.request)
      .then((response) => {
        if (response.status === 0) {
          return response;
        }

        // Clone the response so we can modify headers
        const newHeaders = new Headers(response.headers);

        // Add cross-origin isolation headers
        newHeaders.set("Cross-Origin-Embedder-Policy", "require-corp");
        newHeaders.set("Cross-Origin-Opener-Policy", "same-origin");

        // Also set CORP to allow cross-origin resources
        newHeaders.set("Cross-Origin-Resource-Policy", "cross-origin");

        // Create new response with modified headers
        return new Response(response.body, {
          status: response.status,
          statusText: response.statusText,
          headers: newHeaders,
        });
      })
      .catch((error) => {
        log("SW", "Fetch failed:", error);
        throw error;
      }),
  );
});
