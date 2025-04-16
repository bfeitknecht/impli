module IMP.Eval (State, evalAexp, evalBexp) where

import qualified Data.Map as Map
import Data.Map (Map)
import IMP.Syntax

-- state evaluation
type State = Map Var Val

-- arithmetic expression evaluation
evalAexp :: Aexp -> State -> Val
evalAexp (Bin op e1 e2) st =
    let v1 = evalAexp e1 st
        v2 = evalAexp e2 st
    in case op of
        Add -> v1 + v2
        Sub -> v1 - v2
        Mul -> v1 * v2
evalAexp (Variable x) st =
    case Map.lookup x st of
        Just v  -> v
        Nothing -> 0    -- default zero initialized variable
evalAexp (Numeral n) _ = n

-- boolean expression evaluation
evalBexp :: Bexp -> State -> Bool
evalBexp (Or b1 b2) st = evalBexp b1 st || evalBexp b2 st
evalBexp (And b1 b2) st = evalBexp b1 st && evalBexp b2 st
evalBexp (Not b) st = not (evalBexp b st)
evalBexp (Rel rop e1 e2) st =
    let v1 = evalAexp e1 st
        v2 = evalAexp e2 st
    in case rop of
        Eq  -> v1 == v2
        Neq -> v1 /= v2
        Lt  -> v1 <  v2
        Leq -> v1 <= v2
        Gt  -> v1 >  v2
        Geq -> v1 >= v2
