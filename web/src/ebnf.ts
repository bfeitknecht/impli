/**
 * Railroad diagrams for the IMP grammar in `docs/IMP.ebnf`, composed with
 * railroad-diagrams (the grammar file is the source of truth; this module is
 * a one-to-one transcription which must be kept in sync).
 */
import rr from "railroad-diagrams";

const {
  Diagram,
  Choice,
  Optional,
  OneOrMore,
  ZeroOrMore,
  Sequence,
  Terminal,
  NonTerminal,
} = rr as any;

const T = (text: string) => Terminal(text);
const NT = (text: string) => NonTerminal(text);

const letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  .split("")
  .map(T);
const digits = "0123456789".split("").map(T);

const rules: [string, unknown][] = [
  ["Letter", Choice(0, ...letters)],
  ["Digit", Choice(0, ...digits)],
  [
    "Ident",
    Sequence(NT("Letter"), ZeroOrMore(Choice(0, NT("Letter"), NT("Digit")))),
  ],
  ["Var", Choice(0, NT("Ident"), T("_"))],
  ["Numeral", Sequence(Optional(T("-")), OneOrMore(NT("Digit")))],
  ["Bool", Choice(0, T("true"), T("false"))],
  ["Aop", Choice(0, T("+"), T("-"), T("*"), T("/"), T("%"))],
  [
    "Aexp",
    Sequence(NT("Aterm"), ZeroOrMore(Sequence(NT("Aop"), NT("Aterm")))),
  ],
  [
    "Aterm",
    Choice(
      0,
      NT("Ident"),
      NT("Numeral"),
      Sequence(T("time"), NT("Stm")),
      Sequence(T("("), NT("Aexp"), T(")")),
    ),
  ],
  ["Rop", Choice(0, T("="), T("#"), T("<"), T("<="), T(">"), T(">="))],
  ["Bop", Choice(0, T("and"), T("or"))],
  [
    "Bexp",
    Sequence(NT("Bterm"), ZeroOrMore(Sequence(NT("Bop"), NT("Bterm")))),
  ],
  [
    "Bterm",
    Choice(
      0,
      NT("Bool"),
      Sequence(T("not"), NT("Bexp")),
      Sequence(NT("Aexp"), NT("Rop"), NT("Aexp")),
      Sequence(T("("), NT("Bexp"), T(")")),
    ),
  ],
  ["Dop", Choice(0, T(":="), T("+="), T("-="), T("*="), T("/="), T("%="))],
  [
    "Stm",
    Choice(
      0,
      T("skip"),
      Sequence(NT("Var"), NT("Dop"), NT("Aexp")),
      Sequence(NT("Stm"), T(";"), NT("Stm")),
      Sequence(T("if"), NT("Bexp"), T("then"), NT("Stm"), T("else"), NT("Stm"), T("end")),
      Sequence(T("while"), NT("Bexp"), T("do"), NT("Stm"), T("end")),
      Sequence(T("print"), NT("Aexp")),
      Sequence(T("read"), NT("Ident")),
      Sequence(T("let"), NT("Ident"), T(":="), NT("Aexp"), T("in"), NT("Stm"), T("end")),
      Sequence(NT("Stm"), T("par"), NT("Stm")),
      Sequence(NT("Stm"), T("[]"), NT("Stm")),
      Sequence(T("procedure"), NT("Ident"), T("("), NT("Params"), T(";"), NT("Rets"), T(")"), T("begin"), NT("Stm"), T("end")),
      Sequence(NT("Ident"), T("("), NT("Args"), T(";"), NT("Rets"), T(")")),
      Sequence(T("do"), NT("Stm"), T("until"), NT("Bexp"), T("end")),
      Sequence(T("for"), NT("Ident"), T(":="), NT("Aexp"), T("to"), NT("Aexp"), T("do"), NT("Stm"), T("end")),
      Sequence(T("repeat"), NT("Aexp"), T("times"), NT("Stm"), T("end")),
      Sequence(T("revert"), NT("Stm"), T("if"), NT("Bexp"), T("end")),
      T("break"),
      Sequence(T("match"), NT("Aexp"), T("on"), ZeroOrMore(Sequence(NT("Numeral"), T(":"), NT("Stm"), T(","))), T("default"), T(":"), NT("Stm"), T("end")),
      Sequence(T("havoc"), NT("Ident")),
      Sequence(T("assert"), NT("Bexp")),
      Sequence(T("flip"), T("("), NT("Numeral"), T(")"), NT("Stm"), T("flop"), NT("Stm"), T("end")),
      Sequence(T("raise"), NT("Aexp")),
      Sequence(T("try"), NT("Stm"), T("catch"), NT("Ident"), T("with"), NT("Stm"), T("end")),
      Sequence(T("swap"), NT("Ident"), NT("Ident")),
      Sequence(NT("Stm"), T("alternate"), NT("Stm")),
      Sequence(T("timeout"), NT("Stm"), T("after"), NT("Aexp"), T("end")),
      Sequence(T("("), NT("Stm"), T(")")),
    ),
  ],
  [
    "Args",
    Optional(Sequence(NT("Aexp"), ZeroOrMore(Sequence(T(","), NT("Aexp"))))),
  ],
  [
    "Params",
    Optional(Sequence(NT("Ident"), ZeroOrMore(Sequence(T(","), NT("Ident"))))),
  ],
  ["Rets", Optional(Sequence(NT("Var"), ZeroOrMore(Sequence(T(","), NT("Var")))))],
];

let cache: [string, string][] | null = null;

/** Render all grammar rules to `[name, svg]` pairs, computed once lazily. */
export function renderEbnf(): [string, string][] {
  if (!cache) {
    cache = rules.map(([name, item]) => [
      name,
      (Diagram as any)(item).toString(),
    ]);
  }
  return cache;
}
