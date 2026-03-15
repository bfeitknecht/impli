## `IMP` Semantics

This directory contains the two semantic models for the IMP language.

### Modules

| Module                        | Description                                                                                                 |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------- |
| [`Natural`](Natural.hs)       | Big-step (natural) semantics — executes a statement in one step to the final state                          |
| [`Structural`](Structural.hs) | Small-step (structural) semantics — executes a statement step by step, exposing intermediate configurations |

### Overview

Both modules implement the semantics defined in the FMFP course at ETH Zürich and documented in [`docs/paper/IMP.pdf`](../../../docs/paper/IMP.pdf).

#### Natural Semantics (`Natural.hs`)

Implements the big-step judgment $\langle s, \sigma \rangle \to \sigma'$, mapping a statement and initial state directly to the final state.

```
run :: (Stm, State) -> IMP State
```

This is the default execution model (`Config.structural = False`). It uses less memory since it does not maintain a state stack.

#### Structural Semantics (`Structural.hs`)

Implements the small-step judgment $\langle s, \sigma \rangle \underset{1}{\to} \gamma$, reducing one step at a time and returning the next configuration.

```
step  :: (Stm, [State]) -> IMP Conf   -- single reduction step
steps :: (Stm, [State]) -> IMP State  -- repeated application until completion
```

The state stack enables step-by-step debugging and exposure of intermediate configurations. It is enabled by setting `Config.structural = True`.

#### Unsupported Statements

Some statements (e.g., `Timeout`, `Alternate`) are only supported by one of the two models. Invoking an unsupported statement raises an error via `errata`.
