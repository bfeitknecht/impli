{-# LANGUAGE FlexibleContexts #-}

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
is programmed against an abstract effect signature over 'Input', 'Output' and
'Choose' effects plus 'Error' 'Exception', interpreted by an engine or
frontend.
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
    input,
    output,
    choose,
    fate,
    run,
) where

import Control.Algebra (Has)
import Control.Effect.Error (Error)

import IMP.Exception (Exception)
import IMP.Semantics.Natural (Choose (..), Input (..), Output (..), choose, fate, input, output)
import IMP.Semantics.Structural
import IMP.State (Gamma, State)

import qualified IMP.Semantics.Natural as Natural

-- | Natural semantics, computed to completion over an abstract effect carrier.
run :: (Has (Error Exception) sig m, Has Choose sig m, Has Input sig m, Has Output sig m) => Gamma -> m State
run = Natural.run
