/**
 * Protocol types for the JSON envelopes exchanged with the impli WASM binary.
 *
 * The shapes mirror the aeson encoders in `src/IMP/JSON.hs`; the frontend
 * sends plain strings and receives these JSON structures.
 */

export interface Envelope {
  events: string[];
  status: Status;
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
  | { tag: "error"; fault: Fault };

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

export interface StepsJson {
  steps: StepJson[];
  profile: Profile;
}

export interface TraceEntryJson {
  index: number;
  source: string;
  ast: StmNode;
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
