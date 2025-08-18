module IMP2.Semantic.Structural where

import IMP2.State
import IMP2.Syntax

run :: (State, Stm) -> IMP State
run (state, Skip) = return state
