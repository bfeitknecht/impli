## IMP Semantics

This directory contains the two semantic models for the IMP language.

### Modules

| Module                        | Description                                                                                              |
| ----------------------------- | -------------------------------------------------------------------------------------------------------- |
| [`Structural`](Structural.hs) | Small-step (structural) semantics — one pure transition at a time over configurations, with suspensions  |
| [`Natural`](Natural.hs)       | Big-step (natural) semantics — reified as a freer monad program over effects                             |

### Overview

Both modules implement the semantics defined in the FMFP course at ETH Zürich and documented in [`docs/paper/IMP.pdf`](../../../docs/paper/IMP.pdf).

#### Structural Semantics (`Structural.hs`)

Implements the small-step judgment $\langle s, \sigma \rangle \underset{1}{\to} \gamma$ as a pair of pure functions on configurations `Gamma = (Stm, State)`.

```
step   :: Gamma -> Configuration                  -- single reduction step
resume :: Gamma -> Resolution -> Configuration    -- unblock a suspension
```

Configurations are `Stepped Step`, `Blocked Suspension`, `Stuck Exception` or
`Terminal State`. Statements that interact with the outside world (`read`,
`havoc`) and non-deterministic or parallel choice block as a `Suspension`
which the frontend resolves externally. Applied inference rules are named in
`Rule`, emitted outputs in `Effect`, so every `Step` is a self-contained
record of the transition.

The machine is stack-free: procedure invocation and local scopes rewrite to
`Return`/`Restore` sequences carrying state snapshots in the program itself,
so suspension and resumption only need the plain configuration.

#### Natural Semantics (`Natural.hs`)

Implements the big-step judgment $\langle s, \sigma \rangle \to \sigma'$ as a freer monad program (`IMP.Freer`) over the effects `Input`, `Output`, `Choose` and `Error Exception`.

```
run    :: Gamma -> Eff Effects State             -- program, interpreted by an engine
input  :: String -> Eff effs Integer
output :: String -> Eff effs ()
choose :: Eff effs Bool
fate   :: Eff effs Integer
```

This is the default execution model for the native frontend. Non-deterministic
and parallel choice, `havoc` and `read` send effects which `IMP.Engine`
discharges through the frontend's `Handlers`; the `IMP.Kernel` module
specialises the same program to the fixed effect row `Effects`.

#### Coverage

The structural semantics supports **all** statements of the language,
including `Timeout`, `Alternate` and `FlipFlop`. The natural semantics
supports everything except `Timeout` and `Alternate`: bounded and interleaved
execution are inherently small-step notions for which the big-step judgment
has no rules — they raise an error in big-step interpretation, consistent
with previous releases.

Divergence from the legacy implementations: at a suspended `par`, the engine
sequentialises the branches in the chosen order instead of interleaving
small-steps.
