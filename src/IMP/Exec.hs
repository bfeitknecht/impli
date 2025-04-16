module IMP.Exec (execStm, Output) where

import IMP.Syntax
import IMP.Eval
import qualified Data.Map as Map

type Output = [String]
type ExecResult = (State, Output)

execStm :: Stm -> State -> Output -> ExecResult
execStm Skip st out = (st, out)

execStm (Print x) st out =
    let v = evalAexp (Variable x) st
    in (st, out ++ [show v])

execStm (Assign x e) st out =
    let v = evalAexp e st
    in (Map.insert x v st, out)

execStm (Seq s1 s2) st out =
    let (st1, out1) = execStm s1 st out
    in execStm s2 st1 out1

execStm (If b s1 s2) st out =
    if evalBexp b st
        then execStm s1 st out
        else execStm s2 st out

execStm (While b s) st out =
    if evalBexp b st
        then
            let (st1, out1) = execStm s st out
            in execStm (While b s) st1 out1
        else (st, out)
