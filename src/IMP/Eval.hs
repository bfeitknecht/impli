module IMP.Eval where

import qualified Data.Map as Map

import IMP.Syntax

type VarEnv = Map.Map Var Integer
type ProcEnv = Map.Map Var Proc
type State = (VarEnv, ProcEnv)

data Proc = Proc [Var] [Var] Stm

emptyState :: State
emptyState = (Map.empty, Map.empty)

getVar :: Var -> State -> Integer
getVar x (vars, _) = Map.findWithDefault 0 x vars

setVar :: Var -> Integer -> State -> State
setVar x v (vars, procs) = (Map.insert x v vars, procs)

setVars :: [(Var, Integer)] -> State -> State
setVars bindings (vars, procs) = (foldr (uncurry Map.insert) vars bindings, procs)

getProc :: Var -> State -> Maybe Proc
getProc name (_, procs) = Map.lookup name procs

setProc :: Var -> Proc -> State -> State
setProc name def (vars, procs) = (vars, Map.insert name def procs)

evalAexp :: Aexp -> State -> Val
evalAexp (Numeral n) _ = n
evalAexp (Variable x) state = getVar x state
evalAexp (Bin op e1 e2) state =
    let
        v1 = evalAexp e1 state
        v2 = evalAexp e2 state
    in
        case op of
            Add -> v1 + v2
            Sub -> v1 - v2
            Mul -> v1 * v2

evalBexp :: Bexp -> State -> Bool
evalBexp (Boolean b) _ = b
evalBexp (Or b1 b2) state = evalBexp b1 state || evalBexp b2 state
evalBexp (And b1 b2) state = evalBexp b1 state && evalBexp b2 state
evalBexp (Not b) state = not (evalBexp b state)
evalBexp (Rel rop e1 e2) state =
    let
        v1 = evalAexp e1 state
        v2 = evalAexp e2 state
    in
        case rop of
            Eq -> v1 == v2
            Neq -> v1 /= v2
            Lt -> v1 < v2
            Leq -> v1 <= v2
            Gt -> v1 > v2
            Geq -> v1 >= v2
