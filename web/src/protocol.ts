/**
 * Protocol types for the JSON envelopes exchanged with the impli WASM binary.
 *
 * The shapes mirror the aeson codecs in `src/IMP/JSON.hs`. The JavaScript
 * side owns the session: every API call takes the session as JSON and every
 * envelope carries the updated session back. Step histories arrive as
 * per-call deltas in the envelope; their resource profile is derived locally
 * by {@link deriveProfile}.
 */

export interface Envelope {
  events: string[];
  status: Status;
  /** Steps taken during the call that produced this envelope. */
  steps: StepJson[];
  /** Session state after the call, owned by this side of the boundary. */
  session: SessionJson;
}

export type Status =
  | { tag: "done"; state: StateJson; profile: Profile }
  | {
      tag: "await-input";
      variable: string;
      retry: boolean;
      focus?: string;
      stm?: StmNode;
    }
  | {
      tag: "await-havoc";
      variable: string;
      retry: boolean;
      focus?: string;
      stm?: StmNode;
    }
  | { tag: "await-choice"; retry: boolean; focus?: string; stm?: StmNode }
  | { tag: "running" }
  | { tag: "error"; fault: Fault };

/**
 * Machine state. Note the `vars` map is raw: variables whose names start
 * with an underscore are internal bookkeeping of the kernel and are expected
 * to be filtered for display by {@link visibleState}.
 */
export interface StateJson {
  vars: { [name: string]: number };
  procs: { [name: string]: ProcJson };
  break: boolean;
}

export interface ProcJson {
  params: string[];
  returns: string[];
  body: StmNode;
}

export type SuspensionJson =
  | { tag: "await-input"; variable: string }
  | { tag: "await-havoc"; variable: string }
  | { tag: "await-choice" };

export type ConfigurationJson =
  | { tag: "terminal"; state: StateJson }
  | { tag: "stepped"; step: StepJson }
  | { tag: "blocked"; suspension: SuspensionJson }
  | { tag: "stuck"; exception: ExceptionJson };

export interface FocusJson {
  statement: StmNode;
  state: StateJson;
}

export interface FlightJson {
  statement: StmNode;
  focus: FocusJson;
  blocked: ConfigurationJson;
  resolutions: ResolutionJson[];
  start: StateJson;
}

/** Resolution recorded by the engine, enabling exact replay for derivation trees. */
export type ResolutionJson =
  | { tag: "resolve-input"; value: number }
  | { tag: "resolve-havoc"; value: number }
  | { tag: "resolve-choice"; left: boolean };

/** Trace row: the statement, its starting state and the recorded resolutions. */
export interface TraceRowJson {
  statement: StmNode;
  state: StateJson;
  resolutions: ResolutionJson[];
}

/** Serializable session owned by the JavaScript side between API calls. */
export interface SessionJson {
  environment: StateJson;
  flight: FlightJson | null;
  trace: TraceRowJson[];
}

export interface Profile {
  steps: number;
  outputs: number;
  rules: { rule: string; count: number }[];
}

export interface StepJson {
  rule: string;
  statement: StmNode;
  state: StateJson;
  effects: string[];
}

/** Accumulated steps and derived profile of an interpretation, for the UI. */
export interface StepsJson {
  steps: StepJson[];
  profile: Profile;
}

/** Derivation tree of the natural semantics, as produced by `deriveTree`. */
export interface RuleJson {
  /** Rule name as given in the paper. */
  name: string;
  /** Conclusion of the inference rule instance. */
  conclusion: string;
  /** Premises, i.e. the derivations of the substatements. */
  premise: RuleJson[];
}

/** Result of a derivation: the proof tree and its replayed output, or an error. */
export type DerivationJson =
  | { tag: "tree"; tree: RuleJson; stdout: string[] }
  | { tag: "error"; message: string };

/** Trace row of the UI: newest statement first, one-based index. */
export interface TraceEntryJson {
  index: number;
  source: string;
  ast: StmNode;
}

/** Machine state with internal ('_'-prefixed) bookkeeping variables removed. */
export function visibleState(state: StateJson): StateJson {
  return {
    vars: Object.fromEntries(
      Object.entries(state.vars).filter(([name]) => !name.startsWith("_")),
    ),
    procs: state.procs,
    break: state.break,
  };
}

/** Derive the resource profile of an interpretation from its step stream. */
export function deriveProfile(steps: StepJson[]): Profile {
  const histogram = new Map<string, number>();
  let outputs = 0;
  for (const step of steps) {
    histogram.set(step.rule, (histogram.get(step.rule) ?? 0) + 1);
    outputs += step.effects.length;
  }
  return {
    steps: steps.length,
    outputs,
    rules: [...histogram.entries()]
      .map(([rule, count]) => ({ rule, count }))
      .sort((a, b) => a.rule.localeCompare(b.rule)),
  };
}

/** Build the trace view of a session: newest statement first, one-based. */
export function traceViewOf(trace: TraceRowJson[]): TraceEntryJson[] {
  return trace.map((row, index) => ({
    index: index + 1,
    source: stmToSource(row.statement),
    ast: row.statement,
  }));
}

export type Fault =
  | { tag: "runtime"; exception: ExceptionJson }
  | { tag: "invalid-resolution"; resolution: string }
  | { tag: "no-flight" }
  | { tag: "busy" }
  | { tag: "interrupted" };

export type ExceptionJson =
  | { tag: "parse-failure"; message: string }
  | { tag: "assert-failure"; message: string }
  | { tag: "io-failure"; message: string }
  | { tag: "raised"; value: number }
  | { tag: "error"; message: string }
  | { tag: "info"; message: string }
  | { tag: "empty" };

/** Generic tagged JSON node of abstract syntax trees. */
export interface AstNode {
  tag: string;
  [field: string]: unknown;
}

export type StmNode = AstNode;
export type AexpNode = AstNode;
export type BexpNode = AstNode;
export type ConstructNode = AstNode;

const AOPS: { [op: string]: string } = {
  add: "+",
  sub: "-",
  mul: "*",
  div: "/",
  mod: "%",
};

const ROPS: { [op: string]: string } = {
  eq: "=",
  neq: "#",
  lt: "<",
  leq: "<=",
  gt: ">",
  geq: ">=",
};

const DOPS: { [dop: string]: string } = {
  def: ":=",
  inc: "+=",
  dec: "-=",
  prod: "*=",
  quot: "/=",
  rem: "%=",
};

/** Render an arithmetic expression node to source text. */
export function aexpToSource(node: AexpNode): string {
  switch (node.tag) {
    case "val":
      return String(node.value);
    case "var":
      return String(node.name);
    case "bin":
      return `${aexpToSource(node.left as AexpNode)} ${
        AOPS[node.op as string] ?? node.op
      } ${aexpToSource(node.right as AexpNode)}`;
    case "time":
      return `time (${stmToSource(node.statement as StmNode)})`;
    default:
      return node.tag;
  }
}

/** Render a boolean expression node to source text. */
export function bexpToSource(node: BexpNode): string {
  switch (node.tag) {
    case "lit":
      return node.value ? "true" : "false";
    case "or":
      return `${bexpToSource(node.left as BexpNode)} or ${
        bexpToSource(node.right as BexpNode)
      }`;
    case "and":
      return `${bexpToSource(node.left as BexpNode)} and ${
        bexpToSource(node.right as BexpNode)
      }`;
    case "not":
      return `not ${bexpToSource(node.expr as BexpNode)}`;
    case "rel":
      return `${aexpToSource(node.left as AexpNode)} ${
        ROPS[node.op as string] ?? node.op
      } ${aexpToSource(node.right as AexpNode)}`;
    default:
      return node.tag;
  }
}

/** Render a statement node to source text. */
export function stmToSource(node: StmNode): string {
  switch (node.tag) {
    case "skip":
      return "skip";
    case "vardef":
      return `${node.var} ${DOPS[node.op as string] ?? node.op} ${
        aexpToSource(node.expr as AexpNode)
      }`;
    case "seq":
      return `${stmToSource(node.first as StmNode)}; ${
        stmToSource(node.second as StmNode)
      }`;
    case "ifelse":
      return `if ${bexpToSource(node.condition as BexpNode)} then ${
        stmToSource(node.then as StmNode)
      } else ${stmToSource(node.else as StmNode)} end`;
    case "while":
      return `while ${bexpToSource(node.condition as BexpNode)} do ${
        stmToSource(node.body as StmNode)
      } end`;
    case "print":
      return `print ${aexpToSource(node.expr as AexpNode)}`;
    case "read":
      return `read ${node.var}`;
    case "local":
      return `let ${node.var} := ${aexpToSource(node.expr as AexpNode)} in ${
        stmToSource(node.body as StmNode)
      } end`;
    case "par":
      return `${stmToSource(node.left as StmNode)} par ${
        stmToSource(node.right as StmNode)
      }`;
    case "nondet":
      return `${stmToSource(node.left as StmNode)} [] ${
        stmToSource(node.right as StmNode)
      }`;
    case "procdef":
      return `procedure ${node.name}(${(node.params as string[]).join(",")};${
        (node.returns as string[]).join(",")
      }) begin ${stmToSource(node.body as StmNode)} end`;
    case "procinvoc":
      return `${node.name}(${
        (node.arguments as AexpNode[]).map(aexpToSource).join(",")
      };${(node.returns as string[]).join(",")})`;
    case "restore":
      return "restore";
    case "return":
      return `return (${(node.rets as string[]).join(",")};${
        (node.returns as string[]).join(",")
      })`;
    case "break":
      return "break";
    case "revert":
      return `revert ${stmToSource(node.body as StmNode)} if ${
        bexpToSource(node.condition as BexpNode)
      } end`;
    case "match": {
      const branches = (node.branches as { value: number; statement: StmNode }[])
        .map((b) => `${b.value}: ${stmToSource(b.statement)}`)
        .join(", ");
      return `match ${aexpToSource(node.subject as AexpNode)} on ${branches} default: ${
        stmToSource(node.default as StmNode)
      } end`;
    }
    case "havoc":
      return `havoc ${node.var}`;
    case "assert":
      return `assert ${bexpToSource(node.condition as BexpNode)}`;
    case "flipflop":
      return `flip (${node.index}) ${stmToSource(node.flip as StmNode)} flop ${
        stmToSource(node.flop as StmNode)
      } end`;
    case "raise":
      return `raise ${aexpToSource(node.expr as AexpNode)}`;
    case "trycatch":
      return `try ${stmToSource(node.body as StmNode)} catch ${node.var} with ${
        stmToSource(node.handler as StmNode)
      } end`;
    case "swap":
      return `swap ${node.left} ${node.right}`;
    case "timeout":
      return `timeout ${stmToSource(node.body as StmNode)} after ${
        aexpToSource(node.budget as AexpNode)
      } end`;
    case "alternate":
      return `${stmToSource(node.first as StmNode)} alternate ${
        stmToSource(node.second as StmNode)
      }`;
    default:
      return node.tag;
  }
}

/** Format an exception for terminal output, mirroring the Haskell Show instance. */
export function showException(exception: ExceptionJson): string {
  switch (exception.tag) {
    case "parse-failure":
      return `*** ERROR: parse failure in: ${exception.message}`;
    case "assert-failure":
      return `*** ERROR: assertion failure in: ${exception.message}`;
    case "io-failure":
      return `*** ERROR: IO failure in: ${exception.message}`;
    case "raised":
      return `*** ERROR: uncaught exception raised with: ${exception.value}`;
    case "error":
      return `*** ERROR: ${exception.message}`;
    case "info":
      return `+++ INFO: ${exception.message}`;
    case "empty":
      return "";
  }
}

/** Format a fault for terminal output, mirroring the Haskell Show instance. */
export function showFault(fault: Fault): string {
  switch (fault.tag) {
    case "runtime":
      return showException(fault.exception);
    case "invalid-resolution":
      return `*** ERROR: invalid resolution: ${fault.resolution}`;
    case "no-flight":
      return "*** ERROR: no interpretation in progress";
    case "busy":
      return "*** ERROR: interpretation already in progress";
    case "interrupted":
      return "*** ERROR: interrupted";
  }
}
