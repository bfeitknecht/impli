{- |
Module      : IMP.Statement
Description : Statement execution for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides statement execution for the IMP language.
Natural and structural semantics are supported, determined by 'Config.structural'.
-}
module IMP.Statement where

import IMP.Config
import IMP.Semantics.Natural
import IMP.Semantics.Structural
import IMP.State
import IMP.Syntax

-- | Execute statement in state with appropriate semantics.
execute :: (Stm, State) -> IMP State
execute (stm, state) =
    if structural
        then steps (stm, [state])
        else run (stm, state)
