// Main script for impli WASM web interface
// Uses wasm-webterm to run the impli WASM binary in the browser

// Impli subclass of WasmWebTerm that auto-launches the impli REPL
class Impli extends WasmWebTerm.default {
  async activate(xterm) {
    await super.activate(xterm); // sets up addons, registers JS commands
    // Skip the default REPL by not calling this.repl()
    // Instead, directly launch impli WASM
    await this.printWelcomeMessage().then(msg => {
      this._xterm.writeln(msg);
    });
    this.runWasmCommand('impli');
  }
}

// Wait for DOM to be ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

async function registerServiceWorker() {
  // Register service worker to enable Cross-Origin Isolation
  // Required for SharedArrayBuffer and WebWorkers support in wasm-webterm
  if ('serviceWorker' in navigator) {
    try {
      const registration = await navigator.serviceWorker.register('./sw.js', {
        scope: './'
      });
      
      console.log('Service Worker registered successfully:', registration);
      
      // Wait for the service worker to be active
      if (registration.active) {
        return registration.active;
      }
      
      // Wait for the service worker to activate
      return new Promise((resolve) => {
        const serviceWorker = registration.installing || registration.waiting;
        if (serviceWorker) {
          serviceWorker.addEventListener('statechange', (e) => {
            if (e.target.state === 'activated') {
              resolve(e.target);
            }
          });
        } else {
          resolve(registration.active);
        }
      });
    } catch (error) {
      console.error('Service Worker registration failed:', error);
      return null;
    }
  } else {
    console.warn('Service Workers not supported in this browser');
    return null;
  }
}

async function init() {
  // Register service worker first
  const serviceWorker = await registerServiceWorker();
  
  // If service worker was just registered, reload the page to apply headers
  if (serviceWorker && !navigator.serviceWorker.controller) {
    console.log('Service Worker registered, reloading page to apply headers...');
    window.location.reload();
    return;
  }

  // Create terminal instance
  const terminal = new Terminal({
    cursorBlink: true,
    fontFamily: '"CommitMono", "Courier New", monospace',
    fontSize: 13,
    theme: {
      background: '#0a0a0a',
      foreground: '#e0e0e0'
    }
  });

  // Create Impli addon (extends wasm-webterm)
  // The first parameter is the path to predelivered binaries
  const impliAddon = new Impli('./');

  // Load the addon into the terminal
  terminal.loadAddon(impliAddon);

  // Open terminal in the div
  const div = document.getElementById('terminal');
  terminal.open(div);

  // Focus the terminal
  terminal.focus();

  // impli WASM will be automatically launched by the Impli.activate() method
  console.log('Launching impli WASM...');
}
