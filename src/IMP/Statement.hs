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

-- | Supported semantics for statement execution.
data Semantics
    = Natural
    | Structural
    deriving (Eq, Show)

-- | Execute statement in state with appropriate semantics.
execute :: (Stm, State) -> IMP State
execute = executeWith semantics
    where
        semantics =
            if structural
                then Structural
                else Natural

-- | Execute statement in state with explicitly selected semantics.
executeWith :: Semantics -> (Stm, State) -> IMP State
executeWith mode (stm, state) = case mode of
    Structural -> steps (stm, [state])
    Natural -> run (stm, state)
