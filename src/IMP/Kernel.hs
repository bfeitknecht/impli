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
computes over an abstract monad, reaching the outside world only through the
'Handlers' supplied by an engine or frontend.
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
    Handlers (..),
    hoistHandlers,
    run,
) where

import Control.Monad.Except (MonadError)

import IMP.Exception (Exception)
import IMP.Handlers
import IMP.Semantics.Structural
import IMP.State (Gamma, State)

import qualified IMP.Semantics.Natural as Natural

-- | Natural semantics, computed to completion with effects discharged by handlers.
run :: (MonadError Exception m) => Handlers m -> Gamma -> m State
run = Natural.run
