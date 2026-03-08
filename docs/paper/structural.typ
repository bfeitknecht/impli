#import "@preview/curryst:0.6.0": rule
#import "definitions.typ": *


#let Skip = rule(
  name: [=== Skip],
  $conf(tt("skip"), sigma) ->_1 sigma$,
)

#let Assign = rule(
  name: [=== Assign #footnote[
    $a' = cases(
      x op(ast.o) a comma space eq.o op(eq.triple.not) tt(":="),
      a comma "otherwise"
    )$, where $ast.o$ denotes definition operator's corresponding arithmetic operator]],
  $conf(x eq.o a, sigma) ->_1 sigma[x |-> eval(A, a')sigma]$,
)

#let Print = rule(
  name: [=== Print #footnote[$eval(A, a)sigma$ integer output]],
  $conf(tt("print") a, sigma) ->_1 sigma$,
)

#let Read = rule(
  name: [=== Read #footnote[$v$ integer input]],
  $conf(tt("read") x, sigma) ->_1 sigma[x |-> v]$,
)

#let Sequence = rule(
  name: [=== Sequence],
  $conf(s_1, sigma) ->_1 sigma'$,
  $conf(seq(s_1, s_2), sigma') ->_1 conf(s_2, sigma')$,
)

#let Sequences = rule(
  name: [=== Sequences],
  $conf(s_1, sigma) ->_1 conf(s'_1, sigma')$,
  $conf(seq(s_1, s_2), sigma') ->_1 conf(seq(s'_1, s_2), sigma')$,
)

#let If = rule(
  name: [=== If #footnote[$eval(B, b)sigma = #t$]],
  $conf(tt("if") b tt("then") s_1 tt("else") s_2 tt("end"), sigma) ->_1 conf(s_1, sigma)$,
)

#let Else = rule(
  name: [=== Else #footnote[$eval(B, b)sigma = #f$]],
  $conf(tt("if") b tt("then") s_1 tt("else") s_2 tt("end"), sigma) ->_1 conf(s_2, sigma)$,
)

#let While = rule(
  name: [=== While #footnote[$eval(B, b)sigma = #t$ and $sigma("break") = #f$]],
  $conf(tt("while") b tt("do") s tt("end"), sigma) ->_1 conf(tt("if") b tt("then") seq(s, tt("while") b tt("do") s tt("end") tt("else") tt("skip") tt("end")), sigma)$,
)

#let Local = rule(
  name: [=== Local],
  $conf(tt("let") x tt(":=") a tt("in") s tt("end"), sigma) ->_1 (s #tt(";") #tt("restore"), sigma'[x |-> sigma(x)])$,
)

#let Parallel = rule(
  name: [=== Parallel],
  $conf(s_1, sigma) -> sigma_1$,
  $conf(s_2, sigma) -> sigma_2$,
  $conf(s_1 tt("||") s_2, sigma) -> sigma_1 union sigma_2$,
)

#let Nondeterminate = rule(
  name: [=== Nondeterminate],
  $conf(s_1, sigma) -> sigma_1$,
  $conf(s_2, sigma) -> sigma_2$,
  $conf(s_1 tt("[]") s_2, sigma) -> sigma' in {sigma_1, sigma_2}$,
)

#let Procedure = rule(
  name: [=== Procedure],
  $conf(tt("procedure") proc(p, harpoon(x), harpoon(y)) tt("begin") s tt("end"), sigma) -> sigma[p |=> (harpoon(x), harpoon(y), s)]$,
)

#let Invocation = rule(
  name: [=== Invocation #footnote[$sigma(p) = (harpoon(x), harpoon(y), s)$ such that $|harpoon(x)| = |harpoon(a)|$ and $|harpoon(y)| = |harpoon(z)|$]],
  $conf(s, sigma harpoon([x_i |-> eval(A, a_i)sigma])) -> sigma'$,
  $conf(proc(p, harpoon(a), harpoon(z)), sigma) -> sigma harpoon([z_j |-> sigma'(y_j)])$,
)

#let Raise = rule(
  name: [=== Raise],
  $conf(tt("raise") a, sigma) -> bot[eval(A, a)sigma]$,
)

#let Try = rule(
  name: [=== Try],
  $conf(s_1, sigma) -> sigma'$,
  $conf(tt("try") s_1 tt("catch") x tt("in") s_2 tt("end"), sigma) -> sigma'$,
)

#let Catch = rule(
  name: [=== Catch],
  $conf(s_1, sigma) -> bot[v]$,
  $conf(s_2, sigma[x |-> v]) -> sigma''$,
  $conf(tt("try") s_1 tt("catch") x tt("in") s_2 tt("end"), sigma) -> sigma''$,
)

#let Revert = rule(
  name: [=== Revert #footnote[$eval(B, b)sigma' = #t$]],
  $conf(s, sigma) -> sigma'$,
  $conf(tt("revert") s tt("if") b tt("end"), sigma) -> sigma$,
)

#let Break = rule(
  name: [=== Break],
  $conf(tt("break"), sigma) -> sigma["break" = #t]$,
)

#let Pass = rule(
  name: [=== Pass #footnote[$eval(B, b)sigma = #t$]],
  $conf(tt("assert") b, sigma) -> sigma$,
)

#let Fail = rule(
  name: [=== Fail #footnote[$eval(B, b)sigma = #f$]],
  $conf(tt("assert") b, sigma) -> bot[!]$,
)

#let Havoc = rule(
  name: [=== Havoc],
  $conf(tt("havoc") x, sigma) -> sigma[x |-> n]$,
)

#let Swap = rule(
  name: [=== Swap],
  $conf(tt("swap") x space y, sigma) -> sigma[x |-> sigma(y), y |-> sigma(x)]$,
)

#let Do = rule(
  name: [=== Do],
  $conf(seq(s, tt("while") tt("not") b tt("do") s tt("end")), sigma) -> sigma'$,
  $conf(tt("do") s tt("until") b tt("end"), sigma) -> sigma'$,
)

#let For = rule(
  name: [=== For],
  $conf(tt("let") x tt(":=") a_1 tt("in") tt("while") x tt("<") a_2 tt("do") seq(s, x tt("+= 1") tt("end") tt("end")), sigma) -> sigma'$,
  $conf(tt("for") x tt(":=") a_1 tt("to") a_2 tt("do") s tt("end"), sigma) -> sigma'$,
)

#let Repeat = rule(
  name: [=== Repeat #footnote[$eval(A, a)sigma = n$]],
  $conf(tt("for") tt("_times") tt(":= 0") tt("to") n tt("do") s tt("end"), sigma) -> sigma'$,
  $conf(tt("repeat") a tt("times") s tt("end"), sigma) -> sigma'$,
)

#let Flip = rule(
  name: [=== Flip #footnote[$eval(A, tt("_flip_")n)sigma = "0"$]],
  $conf(s_1, sigma) -> sigma_1$,
  $conf(tt("flip(")n#tt(")") s_1 tt("flop") s_2 tt("end"), sigma) -> sigma_1[tt("_flip_")n |->1]$,
)

#let Flop = rule(
  name: [=== Flop #footnote[$eval(A, tt("_flip_")n)sigma = "1"$]],
  $conf(s_2, sigma) -> sigma_2$,
  $conf(tt("flip(")n#tt(")") s_1 tt("flop") s_2 tt("end"), sigma) -> sigma_2[#tt("_flip_")n |-> 0]$,
)

#let Case = rule(
  name: [=== Case #footnote[$eval(A, a)sigma = v_k$]],
  $conf(s_k, sigma) -> sigma'$,
  $conf(tt("match") a tt("with") harpoon(v_i#tt(":") s_i#tt(",")) tt("default:") d, sigma) -> sigma'$,
)

#let Default = rule(
  name: [=== Default #footnote[$eval(A, a)sigma in.not {v_i}_(i in [m])$]],
  $conf(d, sigma) -> sigma'$,
  $conf(tt("match") a tt("with") harpoon(v_i#tt(":") s_i#tt(",")) tt("default:") d, sigma) -> sigma'$,
)

= Structural Semantics

== #IMP

Again, the standard inference rules of #IMP with IO statements.
#layout(
  (Skip, Assign),
  (Print, Read),
  (Sequence, Sequences),
  If,
  Else,
  While,
)

#pagebreak()

== #EXT

Some various rules for extensions to #IMP.
#layout(
  Local,
  Parallel,
  Nondeterminate,
)
