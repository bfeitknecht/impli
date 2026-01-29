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
  const terminal = new Terminal({
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
  const driver = new WasmWebTerm.default('./');

  // Load the addon into the terminal
  terminal.loadAddon(driver);

  // Open terminal in the div
  const div = document.getElementById('terminal');
  terminal.open(div);

  // Focus the terminal
  terminal.focus();

  // Auto-launch impli.wasm on startup
  // Wait for terminal to be ready before executing
  const TERMINAL_READY_DELAY = 100; // ms - allows terminal to finish initialization
  console.log('Launching impli WASM...');
  
  setTimeout(() => {
    driver.exec('impli');
  }, TERMINAL_READY_DELAY);
}
