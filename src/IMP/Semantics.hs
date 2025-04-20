module IMP.Semantics where

import Control.Concurrent.Async (concurrently)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Syntax

data Proc = Proc [Var] [Var] Stm deriving (Show)
type VarEnv = Map.Map Var Integer
type ProcEnv = Map.Map Var Proc
type State = (VarEnv, ProcEnv)

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

execStm :: Stm -> State -> IO State
execStm stm state@(vars, procs) = case stm of
    Skip -> do return state
    Print e -> do
        let v = evalAexp e state
        print v
        return state
    Def x e -> do
        let v = evalAexp e state
        return $ setVar x v state
    Seq s1 s2 -> do
        state' <- execStm s1 state
        execStm s2 state'
    If b s1 s2 ->
        if evalBexp b state
            then execStm s1 state
            else execStm s2 state
    While b s ->
        if evalBexp b state
            then execStm s state >>= execStm (While b s)
            else return state
    Local x e s -> do
        let old = getVar x state
        let new = evalAexp e state
        let local = setVar x new state
        (vars', procs') <- execStm s local
        return (Map.insert x old vars', procs')
    NonDet s1 s2 -> do
        choice <- (randomIO :: IO Bool)
        if choice
            then execStm s1 state
            else execStm s2 state
    Par s1 s2 -> do
        ((vs1, ps1), (vs2, ps2)) <-
            concurrently
                (execStm s1 state)
                (execStm s2 state)
        -- NOTE: Map.union is left-biased; state1 overrides state2
        let vars' = Map.union vs1 vs2
        let procs' = Map.union ps1 ps2
        return (vars', procs')
    ProcDef name parameters returns body ->
        return $ setProc name (Proc parameters returns body) state
    ProcInvoc name arguments returns ->
        case Map.lookup name procs of
            Nothing -> error $ "Undefined procedure: " ++ name
            Just (Proc params rets s) -> do
                let vals = map (`evalAexp` state) arguments -- evaluate arguments
                let local = (Map.fromList (zip params vals), procs) -- into local environment
                state'@(_, procs') <- execStm s local -- execute body
                let extracts = zip returns (map (`getVar` state') rets) -- extract return values from local state
                let rets' = foldr (uncurry Map.insert) vars extracts -- insert into previous environment
                return (rets', procs')
    _ -> error $ "Unrecognized statement type"
