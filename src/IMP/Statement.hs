{- |
Module      : IMP.Statement
Description : Statement execution for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides statement execution for the IMP language.
Structural and operational semantics are supported, determined by 'Config.operational'.
-}
module IMP.Statement where

import IMP.Config
import IMP.Semantics.Operational
import IMP.Semantics.Structural
import IMP.State
import IMP.Syntax

-- | Execute statement in state with appropriate semantics.
execute :: (Stm, State) -> IMP State
execute (stm, state) =
    if operational
        then steps (stm, [state])
        else run (stm, state)
