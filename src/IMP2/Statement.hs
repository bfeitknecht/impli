module IMP2.Statement where

import Config
import IMP2.Semantic.Operational
import IMP2.Semantic.Structural
import IMP2.State
import IMP2.Syntax

-- | __TODO__
interpret :: State -> Stm -> IMP State
interpret state stm =
    if operational
        then steps ([state], stm)
        else run (state, stm)
