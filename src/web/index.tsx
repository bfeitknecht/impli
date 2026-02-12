import { render, Component } from "npm:preact";
import "@/styles.css";

class App extends Component {
  render({}, {}) {
    return <div id="terminal"></div>;
  }
}

render(<App />, document.body);
