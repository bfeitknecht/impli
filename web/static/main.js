// Main script for impli WASM web interface
// Uses wasm-webterm to run the impli WASM binary in the browser

// Wait for DOM to be ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

function init() {
  // Create terminal instance
  const term = new Terminal({
    cursorBlink: true,
    fontFamily: '"CommitMono", "Courier New Bold", monospace',
    fontSize: 13,
    theme: {
      background: '#0a0a0a',
      foreground: '#e0e0e0'
    }
  });

  // Create wasm-webterm addon
  // The first parameter is the path to predelivered binaries
  const wasmterm = new WasmWebTerm.default('./');

  // Load the addon into the terminal
  term.loadAddon(wasmterm);

  // Open terminal in the container
  const container = document.getElementById('terminal');
  term.open(container);

  // Focus the terminal
  term.focus();

  // Note: When a WASM binary named 'impli.wasm' is placed in the web/static directory,
  // users can run it by typing 'impli' in the terminal.
  // The binary will be automatically fetched and executed.

  console.log('impli WASM terminal initialized');
  console.log('Place impli.wasm in the same directory to enable execution');
}
