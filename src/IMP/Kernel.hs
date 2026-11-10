{-# LANGUAGE DataKinds #-}

{- |
Module      : IMP.Kernel
Description : Pure kernel API of the IMP language
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Facade for the pure kernel semantics of the IMP language.

The structural (small-step) semantics of "IMP.Semantics.Structural" is a pair
of serialisable functions on configurations matching the formal semantics:
'step' performs one transition and 'resume' unblocks a suspended 'Gamma' with
a 'Resolution'. The natural (big-step) semantics of "IMP.Semantics.Natural"
is reified as a freer monad program over 'Input', 'Output' and 'Choose'
effects plus 'Error' 'Exception', interpreted by an engine or frontend.
-}
module IMP.Kernel (
    Gamma,
    Suspension (..),
    Resolution (..),
    Effect (..),
    Rule (..),
    Step (..),
    Configuration (..),
    step,
    resume,
    Input (..),
    Output (..),
    Choose (..),
    Effects,
    input,
    output,
    choose,
    fate,
    run,
) where

import IMP.Freer (Eff)
import IMP.Semantics.Natural (Choose (..), Effects, Input (..), Output (..), choose, fate, input, output)
import IMP.Semantics.Structural
import IMP.State (Gamma, State)

import qualified IMP.Semantics.Natural as Natural

-- | Natural semantics, freer monad program interpreted to completion.
run :: Gamma -> Eff Effects State
run = Natural.run
