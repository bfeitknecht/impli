// Main script for impli WASM web interface
// Uses wasm-webterm to run the impli WASM binary in the browser

// Wait for DOM to be ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', init);
} else {
  init();
}

async function init() {
  // Create terminal instance
  const term = new Terminal({
    cursorBlink: true,
    fontFamily: '"CommitMono", "Courier New", monospace',
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

  // Auto-launch impli.wasm on startup
  console.log('Launching impli WASM...');
  
  // Wait a moment for terminal to be ready, then execute impli
  setTimeout(() => {
    wasmterm.exec('impli');
  }, 100);
}
