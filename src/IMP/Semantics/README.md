## IMP Semantics

The two semantic models of the IMP language, as defined in the FMFP course at
ETH Zürich and documented in [`docs/paper/IMP.pdf`](../../../docs/paper/IMP.pdf).
Both agree on all deterministic programs, which the test suite checks on
generated programs.

#### Structural Semantics (`Structural.hs`)

Implements $\langle s, \sigma \rangle \to_1 \gamma$ as a pure function on
configurations `Gamma = (Term, State)`.

```haskell
step :: Gamma -> Configuration
```

A `Term` is a source statement in runtime context: sequential continuation,
loop iteration, local scope, revertible transaction, procedure call frame
(holding the callee's local state), exception handler, bounded or interleaved
execution. The source syntax thus stays free of internal constructs.

A `Configuration` is one of

- `Stepped Step`, a transition with its residual configuration, emitted output and applied `Rule`,
- `AwaitInput`, `AwaitHavoc`, `AwaitChoice`, blocked with a continuation awaiting the value,
- `Final Flow`, completed normally or by `break`,
- `Stuck Exception`.

`drive` and `run` discharge blocked configurations through `MonadIMP` and run
to completion. `measure` counts steps purely for `time`.

#### Natural Semantics (`Natural.hs`)

Implements $\langle s, \sigma \rangle \to \sigma'$ over `MonadIMP`, returning a
`Flow` so `break` propagates to the innermost loop. `timeout` and `alternate`
have no big-step rules and are delegated to the structural semantics.

```haskell
run :: MonadIMP m => Stm -> State -> m Flow
```
