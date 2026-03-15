#import "definitions.typ": *


#pagebreak()

== Free Variables

$
  fv : bs("Aexp") union bs("Bexp") union bs("Stm") -> cal(P)(bs("Var"))
$
$fv$ is defined inductively and yields the set of free variables occurring within some construct.

$
  fv(n) & = emptyset \
  fv(x) & = {x} \
  fv(a_1 ast.o a_2) & = fv(a_1) union fv(a_2) \
  fv(tt("time") s) & = fv(s) \
  \ \
  fv(tt("true")) & = emptyset \
  fv(tt("false")) & = emptyset \
  fv(tt("not") b) & = fv(b) \
  fv(b_1 compose.o b_2) & = fv(b_1) union fv(b_2) \
  fv(a_1 lt.o a_2) & = fv(a_1) union fv(a_2) \
  \ \
  fv(tt("skip")) & = emptyset \
  fv(x eq.o a) & = {x} union fv(a) \
  fv(seq(s_1, s_2)) & = fv(s_1) union fv(s_2) \
  fv(tt("if") b tt("then") s_1 tt("else") s_2 tt("end")) & = fv(b) union fv(s_1) union fv(s_2) \
  fv(tt("while") b tt("do") s tt("end")) & = fv(b) union fv(s) \
  fv(tt("print") a) & = fv(a) \
  fv(tt("read") x) & = {x} \
  fv(tt("let") x tt(":=") a tt("in") s tt("end")) & = {x} union fv(a) union fv(s) \
  fv(s_1 tt("||") s_2) & = fv(s_1) union fv(s_2) \
  fv(s_1 tt("[]") s_2) & = fv(s_1) union fv(s_2) \
  fv(tt("procedure") proc(p, harpoon(x), harpoon(y)) tt("begin") s tt("end")) & = {p} union {harpoon(x)} union {harpoon(y)} union fv(s) \
  fv(proc(p, harpoon(a), harpoon(z))) & = {p} union {harpoon(z)} union {fv(a_i) | i in [m]} \
  fv(tt("break")) & = emptyset \
  fv(tt("revert") s tt("if") b tt("end")) & = fv(s) union fv(b) \
  fv(tt("match") a tt("with") harpoon(v_i#tt(":") s_i#tt(",")) tt("default:") d tt("end")) & = fv(a) union fv(d) union {fv(s_i) | i in [m]} \
  fv(tt("havoc") x) & = {x} \
  fv(tt("assert") b) & = fv(b) \
  fv(tt("flip(")n#tt(")") s_1 tt("flop") s_2 tt("end")) & = fv(s_1) union fv(s_2) \
  fv(tt("raise") a) & = fv(a) \
  fv(tt("try") s_1 tt("catch") x tt("in") s_2 tt("end")) & = {x} union fv(s_1) union fv(s_2) \
  fv(tt("swap") x space y) & = {x, y} \
  fv(tt("timeout") s tt("after") a tt("end")) & = fv(s) union fv(a) \
  fv(tt("alternate") s_1 space s_2) & = fv(s_1) union fv(s_2)
$
