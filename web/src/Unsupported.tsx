import { Component } from "preact";

export class Unsupported extends Component {
  override render() {
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
          Your browser doesn't support all features required to run {
            <strong>
              <code>impli</code>
            </strong>
          } in the web.
        </p>

        <h2>Required Features</h2>
        <ul>
          <li>JavaScript</li>
          <li>WebAssembly</li>
          <li>ES6 Modules</li>
        </ul>

        <h2>Minimum Browser Versions</h2>
        <ul>
          <li>
            <strong>Chrome/Edge:</strong> 71+ (December 2018)
          </li>
          <li>
            <strong>Firefox:</strong> 68+ (July 2019)
          </li>
          <li>
            <strong>Safari:</strong> 15.0+ (September 2021)
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
