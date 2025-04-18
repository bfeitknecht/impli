module IMP.Exec (execStm) where

import Control.Concurrent.Async (concurrently)
import System.Random (randomIO)

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
execStm (Local x e s) state = do
    let v = evalAexp (Variable x) state
    let v' = evalAexp e state
    let local = Map.insert x v' state
    state' <- execStm s local
    return (Map.insert x v state')
execStm (NonDet s1 s2) state = do
    choice <- (randomIO :: IO Bool)
    if choice
        then execStm s1 state
        else execStm s2 state
execStm (Par s1 s2) state = do
    (state1, state2) <-
        concurrently
            (execStm s1 state)
            (execStm s2 state)
    -- NOTE: Map.union is left-biased; state2 overrides state1
    let state' = Map.union state2 state1
    return state'
