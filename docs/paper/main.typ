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

#show raw: set text(font: "CommitMono Nerd Font Mono")


= Semantics

Let the sets below denote the domains of their respective terms on the meta level.
$
          a, a', ... & in bs("Aexp") \
              b, ... & in bs("Bexp") \
        x, y, z, ... & in bs("Var") \
              v, ... & in bs("Val") = ZZ \
              n, ... & in bs("Numeral") \
              s, ... & in bs("Stm") \
  sigma, sigma', ... & in bs("State") \
              P, ... & in bs("Procedure") = bs("Var")^* times bs("Var")^* times bs("Stm") \
               ast.o & in {#("+", "-", "*", "/", "%").map(tt).join(", ")} \
           compose.o & in {#("not", "and", "or").map(tt).join(", ")} \
                eq.o & in {#(":=", "+=", "-=", "*=", "/=", "%=").map(tt).join(", ")} \
                lt.o & in {#("=", "#", "<", "<=", ">", ">=").map(tt).join(", ")}
$

For #IMP + #EXT, the state domain is defined as triple of variables, procedures, and break flag. Exceptions are denoted with $bot$ and some integer status code or irecoverable failure symbol.
$
  bs("State") & = (bs("Var") -> bs("Val")) times (bs("Var") -> bs("Procedure")) times {#t, #f} union {bot} times (ZZ union {!})
$

Furthermore, the following equivalences hold. Un-circled operators denote their respective arithmetic or boolean operations or relations.
$
                sigma[x |-> a](x) & = eval(A, a)sigma \
                sigma[p |=> P](p) & = P \
        eval(B, tt("not") b)sigma & = not eval(B, b)sigma \
      eval(A, a_1 ast.o a_2)sigma & = eval(A, a_1)sigma ast eval(A, a_2)sigma \
  eval(B, b_1 compose.o b_2)sigma & = eval(B, b_1)sigma compose eval(B, b_2)sigma, compose.o != tt("not") \
       eval(B, a_1 lt.o a_2)sigma & = eval(A, a_1)sigma lt eval(A, a_2)sigma
$

#pagebreak()

#include "natural.typ"

/*
#pagebreak()

#include "structural.typ"
*/
