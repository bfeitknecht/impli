import { Component } from "preact";

/**
 * Requirement check logic adapted from main.js backup.
 */
export function checkRequirements() {
  const features: Record<string, boolean> = {
    "WebAssembly": typeof WebAssembly !== "undefined",
    "SharedArrayBuffer": typeof SharedArrayBuffer !== "undefined",
    "Atomics": typeof Atomics !== "undefined",
    "Web Workers": typeof Worker !== "undefined",
    "Service Workers": "serviceWorker" in navigator,
    "ES6 Modules": "noModule" in HTMLScriptElement.prototype,
    "Cross-Origin Isolation": globalThis.crossOriginIsolated === true,
  };

  // Check for SharedArrayBuffer explicitly if cross-origin isolation is not enabled
  if (!features["Cross-Origin Isolation"] && !features["SharedArrayBuffer"]) {
    try {
      // @ts-ignore: Detecting SharedArrayBuffer existence in non-isolated context
      new SharedArrayBuffer(1);
      features["SharedArrayBuffer"] = true;
    } catch {
      // Not supported in this context
    }
  }

  const missing = Object.entries(features)
    .filter(([_, supported]) => !supported)
    .map(([name]) => name);

  return {
    supported: missing.length === 0,
    features,
    missing,
  };
}

export class Unsupported
  extends Component<{ check: ReturnType<typeof checkRequirements> }> {
  override render() {
    const { check } = this.props;

    return (
      <div
        style={{
          maxWidth: "800px",
          margin: "0 auto",
          padding: "2rem",
          fontFamily: "system-ui, sans-serif",
        }}
      >
        <h1>Browser Not Supported</h1>

        <p>
          Your browser doesn't support all features required to run
          <strong>
            <code>impli</code>
          </strong>{" "}
          in the web.
        </p>

        {check.missing.length > 0 && (
          <div
            style={{
              border: "1px solid #ff5555",
              padding: "1rem",
              borderRadius: "4px",
              margin: "1rem 0",
            }}
          >
            <h2 style={{ color: "#ff5555", marginTop: 0 }}>Missing Features</h2>
            <ul style={{ color: "#ff5555" }}>
              {check.missing.map((feature) => <li key={feature}>{feature}</li>)}
            </ul>
          </div>
        )}

        <h2>Required Features</h2>
        <ul>
          <li>JavaScript</li>
          <li>WebAssembly</li>
          <li>ES6 Modules</li>
          <li>Web Workers</li>
          <li>SharedArrayBuffer</li>
          <li>Atomics</li>
          <li>Service Workers</li>
          <li>Cross-Origin Isolation</li>
        </ul>

        <h2>Minimum Browser Versions</h2>
        <ul>
          <li>
            <strong>Chrome/Edge:</strong> 92+ (July 2021)
          </li>
          <li>
            <strong>Firefox:</strong> 95+ (December 2021)
          </li>
          <li>
            <strong>Safari:</strong> 15.2+ (December 2021)
          </li>
        </ul>

        <p>
          Please use a modern browser or run impli from the command line. Check
          it out on <a href="https://github.com/bfeitknecht/impli">GitHub</a>.
        </p>
      </div>
    );
  }
}
