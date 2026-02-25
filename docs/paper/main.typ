#import "definitions.typ": *


#set page(
  numbering: "1/1",
  number-align: right,
)

#set document(
  title: "IMP Inference Rules",
  author: "Basil Feitknecht",
  description: "Operational Semantics Inference Rules for IMP",
)

#show raw: set text(font: "CommitMono Nerd Font Mono", size: 1em)


= Semantics

Let the sets below denote the domains of their respective terms on the meta level.
$
          a, a', ... & in sans("Aexp") \
              b, ... & in sans("Bexp") \
        x, y, z, ... & in sans("Var") \
              v, ... & in sans("Val") \
           n, i, ... & in sans("Numeral") \
              s, ... & in sans("Stm") \
  sigma, sigma', ... & in sans("State") \
               ast.o & in {#("+", "-", "*", "/", "%").map(tt).join(", ")} \
           compose.o & in {#("not", "and", "or").map(tt).join(", ")} \
                eq.o & in {#(":=", "+=", "-=", "*=", "/=", "%=").map(tt).join(", ")} \
                lt.o & in {#("=", "#", "<", "<=", ">", ">=").map(tt).join(", ")}
$

Specifically, these equalities hold, with $sans("Procedures")$ being an appropriate encapsulation.
$
  sans("Val") = sans("Numeral") & = ZZ \
                  sans("State") & = (sans("Var") -> sans("Numeral")) times sans("Procedures") times {#t, #f}
$

Furthermore, the following equivalences hold. Un-circled operators denote their respective arithmetic or boolean operations or relations.
$
            sigma[x |-> a](x) & = cal(A)[|a|]sigma \
            sigma[p |=> s](p) & = s \
        cal(B)[|tt("not") b|] & = not cal(B)[|b|] \
      cal(A)[|a_1 ast.o a_2|] & = cal(A)[|a_1|] ast cal(A)[|a_2|] \
  cal(B)[|b_1 compose.o b_2|] & = cal(B)[|b_1|] compose cal(B)[|b_2|], compose.o != tt("not") \
       cal(B)[|a_1 lt.o a_2|] & = cal(A)[|a_1|] lt cal(A)[|a_2|]
$

#pagebreak()

#include "natural.typ"

/*
#pagebreak()

#include "structural.typ"
*/
