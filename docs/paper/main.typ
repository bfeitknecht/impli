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
     x, y, z, p, ... & in bs("Var") \
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

For #IMP + #EXT, the state domain is defined as triple of variables, procedures, and break flag. Exceptions are denoted with $bot$ and some integer status code or irrecoverable failure symbol.
$
  bs("State") & = (bs("Var") -> bs("Val")) times (bs("Var") -> bs("Procedure")) times {#t, #f} union {bot} times (ZZ union {!})
$

The following semantic functions map syntactic representations to their semantic entities. \
Un-circled operators denote the respective arithmetic or boolean operations or relations.
$
    eval(N, dot) & : bs("Numeral") -> bs("Val") \
  eval(A, dot)\_ & : bs("Aexp") -> bs("State") -> bs("Val") \
  eval(B, dot)\_ & : bs("Bexp") -> bs("State") -> {#t, #f}
$

$cal(N)$ maps a numeral to its corresponding integer value, i.e. $eval(N, tt("42")) = 42$. Its definition is omitted.

$cal(A)$ is defined inductively and maps an arithmetic expression in some state to an integer value.
$
              eval(A, n)sigma & = eval(N, n) \
              eval(A, x)sigma & = sigma(x) \
  eval(A, a_1 ast.o a_2)sigma & = eval(A, a_1)sigma ast eval(A, a_2)sigma
$

$cal(B)$ is defined inductively and maps a boolean expression in some state to a truth value.
$
         eval(B, tt("true"))sigma & = #t \
        eval(B, tt("false"))sigma & = #f \
        eval(B, tt("not") b)sigma & = not eval(B, b)sigma \
  eval(B, b_1 compose.o b_2)sigma & = eval(B, b_1)sigma compose eval(B, b_2)sigma \
       eval(B, a_1 lt.o a_2)sigma & = eval(A, a_1)sigma lt eval(A, a_2)sigma
$

Furthermore, the following functions allow for state updates.
$
    \_[dot |-> dot] & : bs("State") -> bs("Var") -> bs("Val") -> bs("State") \
    \_[dot |=> dot] & : bs("State") -> bs("Var") -> bs("Procedure") -> bs("State") \
  \_["break" = dot] & : bs("State") -> {#t, #f} -> bs("State") \
$

#pagebreak()

#include "natural.typ"

// #pagebreak()

// #include "structural.typ"
