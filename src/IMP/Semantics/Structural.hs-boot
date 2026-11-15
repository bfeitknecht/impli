module IMP.Semantics.Structural where

import IMP.Exception (Exception)
import IMP.State (State)
import IMP.Syntax (Stm)

measure :: Stm -> State -> Either Exception Integer
