import { Component, createRef, h, render } from "preact";
import htm from "htm";
import { Unsupported } from "@/Unsupported.ts";
import { Impli, SessionView } from "@/impli.ts";
import { StmNode, stmToSource } from "@/protocol.ts";
import { AstTree } from "@/ast.ts";
import { renderEbnf } from "@/ebnf.ts";

// LSP isn't hip enough to know about HTM
const html = (htm as any).bind(h);

type Tab = "terminal" | "state" | "trace" | "ast" | "ebnf";

const TABS: { id: Tab; label: string }[] = [
  { id: "terminal", label: "Terminal" },
  { id: "state", label: "State" },
  { id: "trace", label: "Trace" },
  { id: "ast", label: "AST" },
  { id: "ebnf", label: "EBNF" },
];

function supported() {
  const hasWasm = typeof WebAssembly === "object" &&
    typeof WebAssembly.instantiateStreaming === "function";
  const hasBigInt64Array = typeof BigInt64Array === "function";
  const hasGlobalThis = typeof globalThis !== "undefined";
  return hasWasm && hasBigInt64Array && hasGlobalThis;
}

interface AppState {
  tab: Tab;
  menuOpen: boolean;
  session: SessionView | null;
  ast: StmNode | null;
  astSource: string;
}

function StateTab({ session }: { session: SessionView | null }) {
  if (!session) return html`<p class="panel-note">awaiting interpreter…</p>`;
  const { state } = session;
  const vars = Object.entries(state.vars);
  const procs = Object.entries(state.procs);
  return html`
    <div class="panel">
      <h3>Variables</h3>
      ${vars.length === 0
        ? html`<p class="panel-note">(empty)</p>`
        : html`
            <table class="kv">
              <tbody>
                ${vars.map(
                  ([name, value]) =>
                    html`<tr>
                      <td class="kv-key">${name}</td>
                      <td> := ${value}</td>
                    </tr>`,
                )}
              </tbody>
            </table>
          `}
      <h3>Procedures</h3>
      ${procs.length === 0
        ? html`<p class="panel-note">(empty)</p>`
        : procs.map(
            ([name, proc]) => html`
              <details class="proc">
                <summary>
                  <code
                    >procedure ${name}(${proc.params.join(",")};${proc.returns
                      .join(",")})</code
                  >
                </summary>
                <pre class="source">${stmToSource(proc.body)}</pre>
              </details>
            `,
          )}
      <h3>Break flag</h3>
      <p><code>${String(state.break)}</code></p>
    </div>
  `;
}

function TraceTab({
  session,
  onAst,
}: {
  session: SessionView | null;
  onAst: (node: StmNode, source: string) => void;
}) {
  if (!session) return html`<p class="panel-note">awaiting interpreter…</p>`;
  const { steps, trace } = session;
  return html`
    <div class="panel">
      <h3>Profile of latest interpretation</h3>
      <p class="panel-note">
        ${steps.profile.steps} steps, ${steps.profile.outputs} outputs${steps
          .profile.rules.length > 0
          ? ", rules: " +
            steps.profile.rules
              .map((r) => `${r.rule}×${r.count}`)
              .join(", ")
          : ""}
      </p>
      <h3>Steps with applied inference rules</h3>
      ${steps.steps.length === 0
        ? html`<p class="panel-note">(none yet)</p>`
        : html`
            <table class="steps">
              <thead>
                <tr>
                  <th>#</th>
                  <th>rule</th>
                  <th>residual</th>
                  <th>effects</th>
                </tr>
              </thead>
              <tbody>
                ${steps.steps.map(
                  (step, i) =>
                    html`<tr
                      class="step"
                      title="state: ${JSON.stringify(step.state.vars)}"
                    >
                      <td>${i + 1}</td>
                      <td><code>${step.rule}</code></td>
                      <td><code>${stmToSource(step.statement)}</code></td>
                      <td>
                        ${step.effects.map(
                          (e) => html`<code class="effect">${e}</code>`,
                        )}
                      </td>
                    </tr>`,
                )}
              </tbody>
            </table>
          `}
      <h3>Statement trace</h3>
      ${trace.length === 0
        ? html`<p class="panel-note">(empty)</p>`
        : trace.map(
            (entry) => html`
              <div class="trace-entry">
                <button
                  class="link"
                  onClick=${() => onAst(entry.ast, entry.source)}
                  title="inspect AST"
                  >${entry.index}</button
                >
                <code class="source">${entry.source}</code>
              </div>
            `,
          )}
    </div>
  `;
}

function AstTab({
  ast,
  astSource,
  impli,
}: {
  ast: StmNode | null;
  astSource: string;
  impli: Impli | null;
}) {
  let input: HTMLInputElement | null = null;
  const parse = () => {
    if (!impli || !input || input.value.trim() === "") return;
    impli.parseAstInput(input.value);
    input.value = "";
  };
  return html`
    <div class="panel">
      <div class="ast-input-row">
        <input
          ref=${(el: HTMLInputElement | null) => (input = el)}
          class="ast-input"
          placeholder="parse an IMP construct into its AST…"
          onKeyDown=${(event: KeyboardEvent) => {
            if (event.key === "Enter") parse();
          }}
        />
        <button onClick=${parse}>Parse</button>
      </div>
      ${ast
        ? html`
            <p class="panel-note"><code>${astSource}</code></p>
            <${AstTree} node=${ast} />
          `
        : html`
            <p class="panel-note">
              No tree yet. Use the input above, the <code>:ast</code>
              metacommand, or an entry of the trace tab.
            </p>
          `}
    </div>
  `;
}

function EbnfTab() {
  return html`
    <div class="panel ebnf">
      ${renderEbnf().map(
        ([name, svg]) => html`
          <h3 id=${`rule-${name}`}>${name}</h3>
          <div
            class="railroad"
            dangerouslySetInnerHTML=${{ __html: svg }}
          />
        `,
      )}
      <p class="panel-note">
        Transcribed from
        <a
          href="https://github.com/bfeitknecht/impli/blob/master/docs/IMP.ebnf"
          target="_blank"
          rel="noopener noreferrer"
          >docs/IMP.ebnf</a
        >.
      </p>
    </div>
  `;
}

export class App extends Component<{}, AppState> {
  private containerRef = createRef<HTMLDivElement>();
  private impli: Impli | null = null;

  override state: AppState = {
    tab: "terminal",
    menuOpen: false,
    session: null,
    ast: null,
    astSource: "",
  };

  override async componentDidMount() {
    if (!supported()) return;

    if (this.containerRef.current) {
      if (document.readyState !== "complete") {
        await new Promise((resolve) => {
          globalThis.addEventListener("load", resolve, { once: true });
        });
      }

      await document.fonts.ready;

      this.impli = new Impli(this.containerRef.current);
      this.impli.onSession = (session) => this.setState({ session });
      this.impli.onAst = (node) => {
        this.setState({
          ast: node,
          astSource: stmToSource(node),
          tab: "ast",
        });
      };
      this.impli.start();
    }
  }

  override componentDidUpdate(_: {}, previous: AppState) {
    if (previous.tab !== "terminal" && this.state.tab === "terminal") {
      this.impli?.refit();
    }
  }

  private setTab(tab: Tab) {
    this.setState({ tab, menuOpen: false });
    if (tab === "terminal") this.impli?.refit();
  }

  override render() {
    if (!supported()) {
      return html`<${Unsupported} />`;
    }

    const { tab, menuOpen, session, ast, astSource } = this.state;

    return html`
      <div class="app">
        <header class="topbar">
          <button
            class="brand"
            title="The IMP Language Interpreter in the browser"
            onClick=${() => this.setTab("terminal")}
            >impli</button
          >
          <button
            class="burger"
            aria-label="menu"
            onClick=${() => this.setState({ menuOpen: !menuOpen })}
            >≡</button
          >
          <nav class=${"tabs" + (menuOpen ? " open" : "")}>
            ${TABS.map(
              (t) => html`
                <button
                  class=${"tab" + (tab === t.id ? " active" : "")}
                  onClick=${() => this.setTab(t.id)}
                  >${t.label}</button
                >
              `,
            )}
          </nav>
        </header>
        <main class="content">
          <div
            class=${"tabpanel" + (tab === "terminal" ? " active" : "")}
          >
            <div id="terminal" ref=${this.containerRef} />
          </div>
          ${tab === "state" &&
          html`<div class="tabpanel active">
            <${StateTab} session=${session} />
          </div>`}
          ${tab === "trace" &&
          html`<div class="tabpanel active">
            <${TraceTab}
              session=${session}
              onAst=${(node: StmNode, source: string) =>
                this.setState({ ast: node, astSource: source, tab: "ast" })}
            />
          </div>`}
          ${tab === "ast" &&
          html`<div class="tabpanel active">
            <${AstTab}
              ast=${ast}
              astSource=${astSource}
              impli=${this.impli}
            />
          </div>`}
          ${tab === "ebnf" &&
          html`<div class="tabpanel active">
            <${EbnfTab} />
          </div>`}
        </main>
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
