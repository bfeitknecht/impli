// Service Worker to enable Cross-Origin Isolation for SharedArrayBuffer
// This is required for wasm-webterm to use WebWorkers instead of prompt() fallback
// See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer

// Install event - activate immediately
self.addEventListener('install', (event) => {
  self.skipWaiting();
});

// Activate event - take control immediately
self.addEventListener('activate', (event) => {
  event.waitUntil(self.clients.claim());
});

// Fetch event - intercept all requests and add COOP/COEP headers
self.addEventListener('fetch', (event) => {
  // Only handle same-origin requests
  if (event.request.url.startsWith(self.location.origin)) {
    event.respondWith(
      fetch(event.request)
        .then((response) => {
          // Clone the response so we can modify headers
          const newHeaders = new Headers(response.headers);
          
          // Add Cross-Origin headers required for SharedArrayBuffer
          newHeaders.set('Cross-Origin-Embedder-Policy', 'require-corp');
          newHeaders.set('Cross-Origin-Opener-Policy', 'same-origin');
          
          // Create new response with modified headers
          return new Response(response.body, {
            status: response.status,
            statusText: response.statusText,
            headers: newHeaders,
          });
        })
        .catch((error) => {
          console.error('Service Worker fetch error:', error);
          // Return a simple error response if fetch fails
          return new Response('Service Worker fetch failed', {
            status: 500,
            statusText: 'Internal Server Error',
            headers: new Headers({
              'Content-Type': 'text/plain',
            }),
          });
        })
    );
  }
});
