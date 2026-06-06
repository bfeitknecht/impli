import { Component, createRef, render } from "preact";
import { html } from "@/html.ts";
import { Unsupported } from "@/Unsupported.ts";
import { Header, Tab } from "@/Header.ts";
import { REPL } from "@/REPL.ts";
import { Tree } from "@/Tree.ts";
import { EBNF } from "@/EBNF.ts";
import { Footer } from "@/Footer.ts";

interface AppState {
  activeTab: Tab;
  theme: "light" | "dark";
}

export class App extends Component<{}, AppState> {
  private replRef = createRef<REPL>();

  override state: AppState = {
    activeTab: "repl",
    theme: typeof globalThis !== "undefined" &&
        globalThis.matchMedia?.("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light",
  };

  override componentDidMount() {
    const observer = new MutationObserver(() => {
      this.replRef.current?.applyTheme();
    });
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["style", "class"],
    });

    const mql = globalThis.matchMedia("(prefers-color-scheme: dark)");
    mql.addEventListener("change", () => {
      this.replRef.current?.applyTheme();
    });
  }

  private isSupported() {
    const hasWasm = typeof WebAssembly === "object" &&
      typeof WebAssembly.instantiateStreaming === "function";
    const hasBigInt64Array = typeof BigInt64Array === "function";
    const hasGlobalThis = typeof globalThis !== "undefined";
    return hasWasm && hasBigInt64Array && hasGlobalThis;
  }

  private handleTabChange = (tab: Tab) => {
    this.setState({ activeTab: tab });
  };

  private handleThemeToggle = () => {
    const next = this.state.theme === "dark" ? "light" : "dark";
    this.setState({ theme: next });
    const root = document.documentElement;
    root.classList.toggle("theme-dark", next === "dark");
    root.classList.toggle("theme-light", next === "light");
  };

  override render() {
    if (!this.isSupported()) {
      return html`
        <${Unsupported} />
      `;
    }

    const { activeTab, theme } = this.state;

    // REPL is always mounted to preserve the live WASM/xterm instance.
    // Inactive panels are hidden via CSS rather than unmounted.
    return html`
      <div class="app">
        <${Header}
          activeTab="${activeTab}"
          theme="${theme}"
          onTabChange="${this.handleTabChange}"
          onToggleTheme="${this.handleThemeToggle}"
        />
        <main class="app-content">
          <div class="${"tab-panel" +
            (activeTab === "repl" ? "" : " tab-panel--hidden")}">
            <${REPL} ref="${this.replRef}" />
          </div>
          <div class="${"tab-panel" +
            (activeTab === "tree" ? "" : " tab-panel--hidden")}">
            <${Tree} />
          </div>
          <div class="${"tab-panel" +
            (activeTab === "ebnf" ? "" : " tab-panel--hidden")}">
            <${EBNF} />
          </div>
        </main>
        <${Footer} />
      </div>
    `;
  }
}

render(
  html`
    <${App} />
  `,
  document.body,
);
