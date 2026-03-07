#import "@preview/curryst:0.6.0": rule
#import "definitions.typ": *


#let Skip = rule(
  name: [=== Skip],
  $conf(tt("skip"), sigma) ->_1 sigma$,
)

#let Assign = rule(
  name: [=== Assign #footnote[
    $a' = cases(
      x op(ast.o) a comma space eq.o op(!=) tt(":="),
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

= Structural Semantics

== #IMP

These constitute the standard inference rules of #IMP with addition of $tt("print")$ and $tt("read")$ to facilitate IO.
#layout(
  (Skip, Assign),
  (Print, Read),
  (Sequence, Sequences),
  If,
  Else,
  While,
)
