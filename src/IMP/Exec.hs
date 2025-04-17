module IMP.Exec (execStm) where

import qualified Data.Map as Map
import IMP.Eval
import IMP.Syntax

execStm :: Stm -> State -> IO State
execStm Skip state = do return state
execStm (Print e) state = do
    let v = evalAexp e state
    print v
    return state
execStm (Assign x e) state = do
    let v = evalAexp e state
    return (Map.insert x v state)
execStm (Seq s1 s2) state = do
    state' <- execStm s1 state
    execStm s2 state'
execStm (If b s1 s2) state =
    if evalBexp b state
        then execStm s1 state
        else execStm s2 state
execStm (While b s) state =
    if evalBexp b state
        then do
            state' <- execStm s state
            execStm (While b s) state'
        else return state
