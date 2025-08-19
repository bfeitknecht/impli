module IMP2.Statement where

import Config
import IMP2.Semantic.Operational
import IMP2.Semantic.Structural
import IMP2.State
import IMP2.Syntax

-- | TODO
interpret :: (Stm, State) -> IMP State
interpret (stm, state) =
    if operational
        then steps (stm, [state])
        else run (stm, state)
