/**
 * Service Worker for impli web REPL
 *
 * GitHub Pages does not support custom HTTP headers, so this service worker
 * is used to inject any necessary response headers at runtime.
 */

import { log } from "@/util.ts";

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
