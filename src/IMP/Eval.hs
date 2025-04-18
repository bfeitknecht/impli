module IMP.Eval where

import Data.Map (Map)

import qualified Data.Map as Map

import IMP.Syntax

type State = Map Var Val

evalAexp :: Aexp -> State -> Val
evalAexp (Numeral n) _ = n
evalAexp (Variable x) state =
    case Map.lookup x state of
        Just v -> v
        Nothing -> 0 -- default zero initialized variable
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
