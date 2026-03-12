#import "@preview/curryst:0.6.0": prooftree, rule


#let layout(column-gutter: 3em, row-gutter: 2em, ..rules) = {
  set align(center)

  // Map over each argument passed to the function
  let rows = rules
    .pos()
    .map(item => {
      if type(item) == array {
        // If the item is an array, join elements horizontally with the column-gutter
        item.map(prooftree).map(box).join(h(column-gutter, weak: true))
      } else {
        // If it's a single item, just wrap it in a box
        box(prooftree(item))
      }
    })

  // Wrap in a block to provide spacing after the layout
  block(
    below: 2em,
    stack(dir: ttb, spacing: row-gutter, ..rows),
  )
}

#let emptyset = $diameter$
#let dot = h(0.2em) + $dot$ + h(0.2em)
#let fv = $"fv"$
#let tt(it) = raw(it)
#let bs(it) = $bold(sans(it))$
#let conf(stm, state) = $chevron.l stm, state chevron.r$
#let seq(s1, s2) = $s1#tt(";") s2$
#let t = tt("true")
#let f = tt("false")
#let eval(X, x) = $cal(#X)[|#x|]$
#let proc(name, args, rets) = {
  let format(val) = {
    if type(val) == array {
      val.join(", ")
    } else {
      val
    }
  }
  let sign(s1, s2) = if rets == () {
    $s1#tt(";")$
  } else {
    seq(s1, s2)
  }
  $name#tt("(")sign(format(args), format(rets))#tt(")")$
}
