import { Component } from "preact";
import { html } from "@/html.ts";
import { Logo } from "@/Logo.ts";

export type Tab = "repl" | "tree" | "ebnf";

interface HeaderProps {
  activeTab: Tab;
  onTabChange: (tab: Tab) => void;
  theme: "light" | "dark";
  onToggleTheme: () => void;
}

const repository = "https://github.com/bfeitknecht/impli";
const paper = "https://bfeitknecht.github.io/impli/IMP.pdf";

const tabs: { id: Tab; label: string }[] = [
  { id: "repl", label: "REPL" },
  { id: "tree", label: "Tree" },
  { id: "ebnf", label: "EBNF" },
];

export class Header extends Component<HeaderProps> {
  override render() {
    const { activeTab, onTabChange, theme, onToggleTheme } = this.props;

    return html`
      <header class="header">
        <div class="header-brand" onClick="${() => onTabChange("repl")}">
          <${Logo} />
        </div>
        <nav class="header-nav">
          ${tabs.map((tab) => {
            const cls = "nav-tab" +
              (activeTab === tab.id ? " nav-tab--active" : "");
            return html`
              <button class="${cls}" onClick="${() => onTabChange(tab.id)}">
                ${tab.label}
              </button>
            `;
          })}
        </nav>
        <div class="header-links">
          <a
            class="header-link"
            href="${repository}"
            target="_blank"
            rel="noopener noreferrer"
          >[gh]</a>
          <a
            class="header-link"
            href="${paper}"
            target="_blank"
            rel="noopener noreferrer"
          >[pdf]</a>
          <span class="header-link" onClick="${() => onToggleTheme()}">
            ${theme === "dark" ? "[●]" : "[○]"}
          </span>
        </div>
      </header>
    `;
  }
}
