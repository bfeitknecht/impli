module IMP.Statement where

import Config
import IMP.Semantic.Operational
import IMP.Semantic.Structural
import IMP.State
import IMP.Syntax

-- | TODO
interpret :: (Stm, State) -> IMP State
interpret (stm, state) =
    if operational
        then steps (stm, [state])
        else run (stm, state)
