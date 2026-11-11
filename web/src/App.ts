import { Component, createRef, h, render } from "preact";
import { useEffect, useState } from "preact/hooks";
import htm from "htm";
import { Unsupported } from "@/Unsupported.ts";
import { Impli, SessionView, initTerminal } from "@/impli.ts";
import {
  DerivationJson,
  RuleJson,
  Status,
  StmNode,
  TraceEntryJson,
  showFault,
  stmToSource,
} from "@/protocol.ts";
import { AstTree } from "@/ast.ts";
import { renderEbnf } from "@/ebnf.ts";

// LSP isn't hip enough to know about HTM
const html = (htm as any).bind(h);

type Tab = "terminal" | "step" | "tree" | "state" | "trace" | "ast" | "ebnf";

const PANEL_TABS: { id: Tab; label: string }[] = [
  { id: "state", label: "State" },
  { id: "step", label: "Step" },
  { id: "tree", label: "Tree" },
  { id: "trace", label: "Trace" },
  { id: "ast", label: "AST" },
  { id: "ebnf", label: "EBNF" },
];

const TABS: { id: Tab; label: string }[] = [
  { id: "terminal", label: "Terminal" },
  ...PANEL_TABS,
];

const WIDE_QUERY = "(min-width: 900px)";

function supported() {
  const hasWasm = typeof WebAssembly === "object" &&
    typeof WebAssembly.instantiateStreaming === "function";
  const hasBigInt64Array = typeof BigInt64Array === "function";
  const hasGlobalThis = typeof globalThis !== "undefined";
  return hasWasm && hasBigInt64Array && hasGlobalThis;
}

interface AppState {
  tab: Tab;
  /** Last viewed side panel; drives the right-hand pane on wide viewports. */
  panel: Tab;
  /** Whether the viewport is wide enough to show the terminal plus a panel. */
  wide: boolean;
  menuOpen: boolean;
  session: SessionView | null;
  /** Newest committed statement; all panels center on it. */
  current: TraceEntryJson | null;
  tree: { index: number; result: DerivationJson } | null;
  ast: StmNode | null;
  astSource: string;
  astMessage: string | null;
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
      ${steps.steps.length >= 1024 &&
      html`<p class="panel-note">showing the latest 1024 steps</p>`}
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
  astMessage,
  impli,
}: {
  ast: StmNode | null;
  astSource: string;
  astMessage: string | null;
  impli: Impli | null;
}) {
  let input: HTMLInputElement | null = null;
  const parse = async () => {
    if (!impli || !input || input.value.trim() === "") return;
    await impli.parseAstInput(input.value);
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
      ${astMessage && html`<p class="panel-note">${astMessage}</p>`}
      ${ast
        ? html`
            <${AstTree} node=${ast} />
          `
        : html`
            <p class="panel-note">
              run a statement in the REPL, pick one from the trace, or parse a
              construct above; its syntax tree appears here.
            </p>
          `}
    </div>
  `;
}

function StepTab({
  session,
  impli,
  prefill,
}: {
  session: SessionView | null;
  impli: Impli | null;
  /** Source of the latest REPL statement, filled in while untouched. */
  prefill: string;
}) {
  const [source, setSource] = useState("");
  const [auto, setAuto] = useState(true);
  useEffect(() => {
    if (auto && prefill !== "") setSource(prefill);
  }, [prefill, auto]);
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

  const start = async () => {
    if (source.trim() === "") return;
    const result = await impli.stepBegin(source);
    if (result) setStatus(result);
  };

  const step = async () => {
    const result = await impli.stepOnce();
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

  const cancel = async () => {
    await impli.stepCancel();
    setStatus(null);
  };

  const submitResolution = async (havoc: boolean) => {
    const value = resolution.trim() === ""
      ? String(Math.floor(Math.random() * 2147483647) - 1073741823)
      : resolution.trim();
    setResolution("");
    const result = await impli.stepResolveInput(value, havoc);
    if (result) setStatus(result);
  };

  const choose = async (left: boolean) => {
    const result = await impli.stepResolveChoice(left);
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
              onInput=${(event: Event) => {
                const value = (event.target as HTMLInputElement).value;
                setSource(value);
                setAuto(value.trim() === "");
              }}
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

function RuleNode({ rule }: { rule: RuleJson }) {
  const premises = rule.premise.map(
    (child) => html`<${RuleNode} rule=${child} />`,
  );
  return html`
    <div class="tree-node">
      ${premises.length > 0 &&
      html`<div class="tree-premises">${premises}</div>`}
      <div class="tree-rule">
        <span class="tree-rule-line"></span>
        <span class="tree-label">${rule.name}</span>
      </div>
      <code class="tree-conclusion">${rule.conclusion}</code>
    </div>
  `;
}

function TreeTab({
  session,
  impli,
  tree,
  current,
  onTree,
}: {
  session: SessionView | null;
  impli: Impli | null;
  tree: { index: number; result: DerivationJson } | null;
  current: TraceEntryJson | null;
  onTree: (tree: { index: number; result: DerivationJson } | null) => void;
}) {
  if (!session || !impli) {
    return html`<p class="panel-note">awaiting interpreter…</p>`;
  }
  const rows = session.trace;
  const derive = async (index: number) => {
    const result = await impli.deriveTree(index);
    if (result) onTree({ index, result });
  };
  return html`
    <div class="panel">
      <h3>Derivation trees</h3>
      <p class="panel-note">
        Natural-semantics proof trees of the statements in the trace,
        replaying exactly the values entered and choices made during their
        execution. Resolutions are only recorded by the structural semantics,
        so trees involving reads, havocs or nondeterministic choices require
        the stepper.
      </p>
      ${rows.length === 0
        ? html`<p class="panel-note">trace is empty</p>`
        : rows.map(
            (row) => html`
              <div
                class=${"trace-entry" +
                (current && row.index === current.index ? " current" : "")}
              >
                <button class="link" onClick=${() => derive(row.index)}>
                  ${current && row.index === current.index
                    ? "derive"
                    : `#${row.index}`}
                </button>
                <code>${row.source}</code>
              </div>
            `,
          )}
      ${tree &&
      html`
        <h3>Tree of #${tree.index}</h3>
        ${tree.result.tag === "error"
          ? html`<p class="panel-note">${tree.result.message}</p>`
          : html`
            ${tree.result.stdout.length > 0 &&
            html`<p class="panel-note">prints:
              <code>${tree.result.stdout.join(", ")}</code></p>`}
            <div class="tree-scroll">
              <${RuleNode} rule=${tree.result.tree} />
            </div>
          `}
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
    panel: "state",
    wide: globalThis.matchMedia?.(WIDE_QUERY).matches ?? false,
    menuOpen: false,
    session: null,
    current: null,
    tree: null,
    ast: null,
    astSource: "",
    astMessage: null,
  };

  /** How many steps a statement may take before trees are not derived implicitly. */
  private static AUTO_DERIVE_STEPS = 256;

  override async componentDidMount() {
    if (!supported()) return;

    globalThis.matchMedia(WIDE_QUERY).addEventListener("change", (event) => {
      this.setState({ wide: event.matches });
      this.impli?.refit();
    });
    globalThis.addEventListener("resize", () => this.impli?.refit());

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
          astMessage: null,
          tab: "ast",
          panel: "ast",
        });
      };
      this.impli.onAstMessage = (message) => this.setState({ astMessage: message });
      this.impli.onStatement = (entry) => {
        this.setState({
          current: entry,
          ast: entry.ast,
          astSource: entry.source,
          astMessage: null,
        });
      };
      this.impli.start();
    }
  }

  override async componentDidUpdate(_: {}, previous: AppState) {
    const hiddenBefore = !previous.wide && previous.tab !== "terminal";
    const visibleNow = this.state.wide || this.state.tab === "terminal";
    if (hiddenBefore && visibleNow) this.impli?.refit();

    // Derive the tree of a freshly committed statement when it is small and
    // the tree panel is looking at it.
    const treePanelActive = this.state.wide
      ? this.state.panel === "tree"
      : this.state.tab === "tree";
    const current = this.state.current;
    if (
      treePanelActive &&
      current &&
      previous.current !== current &&
      this.state.session &&
      !this.state.session.flight &&
      this.state.session.steps.steps.length <= App.AUTO_DERIVE_STEPS &&
      this.impli
    ) {
      const result = await this.impli.deriveTree(1);
      if (result && this.state.current === current) {
        this.setState({ tree: { index: 1, result } });
      }
    }
  }

  private setTab(tab: Tab) {
    this.setState({
      tab,
      menuOpen: false,
      ...(tab === "terminal" ? {} : { panel: tab }),
    });
  }

  /** Panel body for the given side tab. */
  private panelContent(which: Tab) {
    const { session, tree, ast, astSource, astMessage, current } = this.state;
    switch (which) {
      case "step":
        return html`
          <${StepTab}
            session=${session}
            impli=${this.impli}
            prefill=${current?.source ?? ""}
          />
        `;
      case "tree":
        return html`
          <${TreeTab}
            session=${session}
            impli=${this.impli}
            tree=${tree}
            current=${current}
            onTree=${(next: { index: number; result: DerivationJson } | null) =>
              this.setState({ tree: next })}
          />
        `;
      case "state":
        return html`<${StateTab} session=${session} />`;
      case "trace":
        return html`
          <${TraceTab}
            session=${session}
            onAst=${(node: StmNode, source: string) =>
              this.setState({ ast: node, astSource: source, tab: "ast", panel: "ast" })}
          />
        `;
      case "ast":
        return html`
          <${AstTab}
            ast=${ast}
            astSource=${astSource}
            astMessage=${astMessage}
            impli=${this.impli}
          />
        `;
      case "ebnf":
        return html`<${EbnfTab} />`;
      default:
        return null;
    }
  }

  override render() {
    if (!supported()) {
      return html`<${Unsupported} />`;
    }

    const { tab, menuOpen, wide, panel, current } = this.state;
    const activeTab = (id: Tab) => (wide ? panel === id : tab === id);

    return html`
      <div class=${"app" + (wide ? " wide" : "")}>
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
                  class=${"tab" + (activeTab(t.id) ? " active" : "")}
                  onClick=${() => this.setTab(t.id)}
                  >${t.label}</button
                >
              `,
            )}
          </nav>
        </header>
        <main class="content">
          <div
            class=${"pane terminal-pane" +
            (wide || tab === "terminal" ? "" : " hidden")}
          >
            <div id="terminal" ref=${this.containerRef} />
          </div>
          <div
            class=${"pane side-pane" +
            (wide || tab !== "terminal" ? "" : " hidden")}
          >
            <nav class="tabs side-tabs">
              ${PANEL_TABS.map(
                (t) => html`
                  <button
                    class=${"tab" + (activeTab(t.id) ? " active" : "")}
                    onClick=${() => this.setTab(t.id)}
                    >${t.label}</button
                  >
                `,
              )}
            </nav>
            <div class=${"tabpanel" + (tab === "terminal" && !wide ? "" : " active")}>
              ${this.panelContent(wide ? panel : tab)}
            </div>
          </div>
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
