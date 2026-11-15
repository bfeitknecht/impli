{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : IMP.Interpret
Description : Interpretation entrypoint for the IMP language
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Entrypoint for interpretation of statements with either semantics, after
static checks. Frontends use 'execute' and provide 'IMP.Effect.MonadIMP'.
-}
module IMP.Interpret (
    Semantics (..),
    execute,
) where

import Control.Monad.Except (throwError)

import IMP.Check
import IMP.Effect
import IMP.Exception
import IMP.State
import IMP.Syntax

import qualified IMP.Semantics.Natural as Natural
import qualified IMP.Semantics.Structural as Structural

-- | Semantics to interpret statements with.
data Semantics
    = Natural
    | Structural
    deriving (Bounded, Enum, Eq, Show)

-- | Check and execute statement in state with semantics, return final state.
execute :: (MonadIMP m) => Semantics -> Stm -> State -> m State
execute semantics stm state = do
    liftEval (check stm)
    flow <- case semantics of
        Natural -> Natural.run stm state
        Structural -> Structural.run stm state
    case flow of
        Next state' -> return state'
        Broken _ -> throwError $ Error "break outside loop" -- INFO: Unreachable after check
{-# INLINEABLE execute #-}
