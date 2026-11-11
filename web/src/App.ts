import { Component, createRef, h, render } from "preact";
import { useState } from "preact/hooks";
import htm from "htm";
import { Unsupported } from "@/Unsupported.ts";
import { Impli, SessionView, initTerminal } from "@/impli.ts";
import { Status, StmNode, showFault, stmToSource } from "@/protocol.ts";
import { AstTree } from "@/ast.ts";
import { renderEbnf } from "@/ebnf.ts";

// LSP isn't hip enough to know about HTM
const html = (htm as any).bind(h);

type Tab = "terminal" | "step" | "state" | "trace" | "ast" | "ebnf";

const TABS: { id: Tab; label: string }[] = [
  { id: "terminal", label: "Terminal" },
  { id: "step", label: "Step" },
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
  const { state, flight } = session;
  const vars = Object.entries(state.vars);
  const procs = Object.entries(state.procs);
  return html`
    <div class="panel">
      ${flight &&
      html`
        <h3>In flight</h3>
        <p class="panel-note">
          <code>${stmToSource(flight.focus.statement)}</code>
        </p>
        <table class="kv">
          <tbody>
            ${Object.entries(flight.focus.state.vars)
              .filter(([name]) => !name.startsWith("_"))
              .map(
                ([name, value]) => html`<tr>
                  <td class="kv-key">${name}</td>
                  <td> := ${value}</td>
                </tr>`,
              )}
          </tbody>
        </table>
      `}
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

function StepTab({
  session,
  impli,
}: {
  session: SessionView | null;
  impli: Impli | null;
}) {
  const [source, setSource] = useState("");
  const [status, setStatus] = useState<Status | null>(null);
  const [resolution, setResolution] = useState("");
  const [runningAll, setRunningAll] = useState(false);
  if (!session || !impli) {
    return html`<p class="panel-note">awaiting interpreter…</p>`;
  }

  const flight = session.flight;
  const blocked = flight?.blocked;
  const beginning = blocked?.tag === "stepped";
  const awaiting =
    blocked?.tag === "blocked" ? blocked.suspension : null;

  const start = () => {
    if (source.trim() === "") return;
    const result = impli.stepBegin(source);
    if (result) setStatus(result);
  };

  const step = () => {
    const result = impli.stepOnce();
    if (result) setStatus(result);
  };

  const runAll = async () => {
    setRunningAll(true);
    try {
      setStatus(await impli.stepRun());
    } finally {
      setRunningAll(false);
    }
  };

  const stopAll = () => {
    impli.requestStopStepping();
  };

  const cancel = () => {
    impli.stepCancel();
    setStatus(null);
  };

  const submitResolution = (havoc: boolean) => {
    const value = resolution.trim() === ""
      ? String(Math.floor(Math.random() * 2147483647) - 1073741823)
      : resolution.trim();
    setResolution("");
    const result = impli.stepResolveInput(value, havoc);
    if (result) setStatus(result);
  };

  const choose = (left: boolean) => {
    const result = impli.stepResolveChoice(left);
    if (result) setStatus(result);
  };

  const statusLine = () => {
    if (runningAll) return html`<p class="panel-note">running…</p>`;
    if (!status && !flight) return null;
    switch (status?.tag) {
      case "done":
        return html`<p class="panel-note">done — state committed to the environment</p>`;
      case "error":
        return html`<p
          class="panel-note"
          style="color: var(--terminal-scrollback, inherit)"
        >
          ${showFault(status.fault)}
        </p>`;
      case "await-input":
        return html`<p class="panel-note">awaits input for ${status.variable}…</p>`;
      case "await-havoc":
        return html`<p class="panel-note">awaits havoc value for ${status.variable}…</p>`;
      case "await-choice":
        return html`<p class="panel-note">awaits choice…</p>`;
      case "running":
        break;
      default:
        break;
    }
    if (flight) {
      return html`<p class="panel-note">in flight,
        ${session.steps.steps.length} steps taken</p>`;
    }
    return null;
  };

  return html`
    <div class="panel">
      <h3>Small-step interpretation</h3>
      ${!flight
        ? html`
          <div class="ast-input-row">
            <input
              class="ast-input"
              placeholder="begin stepping a statement…"
              value=${source}
              onInput=${(event: Event) =>
                setSource((event.target as HTMLInputElement).value)}
              onKeyDown=${(event: KeyboardEvent) => {
                if (event.key === "Enter") start();
              }}
            />
            <button onClick=${start}>Step</button>
          </div>
        `
        : html`
          <p class="panel-note">
            focus: <code>${stmToSource(flight.focus.statement)}</code>
          </p>
          ${statusLine()}
          <div class="step-controls">
            ${beginning &&
            html`
              <button onClick=${step}>one step</button>
              ${!runningAll
                ? html`<button onClick=${runAll}>run</button>`
                : html`<button onClick=${stopAll}>stop</button>`}
            `}
            ${awaiting?.tag === "await-input" || awaiting?.tag === "await-havoc"
              ? html`
                <div class="ast-input-row">
                  <input
                    class="ast-input"
                    placeholder="${awaiting.variable} := "
                    value=${resolution}
                    onInput=${(event: Event) =>
                      setResolution((event.target as HTMLInputElement).value)}
                    onKeyDown=${(event: KeyboardEvent) => {
                      if (event.key === "Enter") {
                        submitResolution(awaiting.tag === "await-havoc");
                      }
                    }}
                  />
                  <button
                    onClick=${() =>
                      submitResolution(awaiting.tag === "await-havoc")}
                  >
                    resolve
                  </button>
                </div>
              `
              : null}
            ${awaiting?.tag === "await-choice"
              ? html`
                <button onClick=${() => choose(true)}>
                  take left: ${chooseLabel(flight, true)}
                </button>
                <button onClick=${() => choose(false)}>
                  take right: ${chooseLabel(flight, false)}
                </button>
              `
              : null}
            <button onClick=${cancel}>cancel</button>
          </div>
        `}
      <h3>Current state</h3>
      ${flight
        ? html`
          <table class="kv">
            <tbody>
              ${visibleEntries(flight.focus.state.vars).map(
                ([name, value]) => html`<tr>
                  <td class="kv-key">${name}</td>
                  <td> := ${value}</td>
                </tr>`,
              )}
              ${visibleEntries(flight.focus.state.vars).length === 0
                ? html`<tr><td class="panel-note">(empty)</td></tr>`
                : null}
            </tbody>
          </table>
        `
        : html`<p class="panel-note">no interpretation in flight</p>`}
    </div>
  `;
}

/** Visible (non-bookkeeping) variable entries of a state during stepping. */
function visibleEntries(vars: { [name: string]: number }): [string, number][] {
  return Object.entries(vars).filter(([name]) => !name.startsWith("_"));
}

/** Label one side of a choice for the stepping tab. */
function chooseLabel(
  flight: { focus: { statement: StmNode } },
  left: boolean,
): string {
  const node = flight.focus.statement;
  if (node.tag === "nondet" || node.tag === "par") {
    const side = node[left ? "left" : "right"] as StmNode;
    return stmToSource(side);
  }
  return left ? "first" : "second";
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
      await initTerminal();

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
          ${tab === "step" &&
          html`<div class="tabpanel active">
            <${StepTab} session=${session} impli=${this.impli} />
          </div>`}
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
