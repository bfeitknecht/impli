module IMP2.Semantic.Statement where

import Config
import IMP2.Semantic.Operational
import IMP2.Semantic.Structural
import IMP2.Syntax

import IMP2.Semantic.State

interpret :: (State, Stm) -> IMP State
interpret (st, stm) =
    if operational
        then steps ([st], Just stm)
        else run (st, stm)
