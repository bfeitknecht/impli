module IMP.Semantics where

import Control.Concurrent.Async (concurrently)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Pretty
import IMP.Syntax

type Vars = Map.Map Var Integer
type Procs = Map.Map Var Proc
type State = (Vars, Procs)

emptyState :: State
emptyState = (Map.empty, Map.empty)

getVar :: Var -> State -> Integer
getVar x (vars, _) = Map.findWithDefault 0 x vars

setVar :: Var -> Integer -> State -> State
setVar "_" _ state = state -- placeholder write-only variable
setVar x v (vars, procs) = (Map.insert x v vars, procs)

setVars :: [(Var, Integer)] -> State -> State
setVars bindings state = foldr (uncurry setVar) state bindings

getProc :: Var -> State -> Maybe Proc
getProc name (_, procs) = Map.lookup name procs

setProc :: Var -> Proc -> State -> State
setProc name def (vars, procs) = (vars, Map.insert name def procs)

evalAexp :: Aexp -> State -> Val
evalAexp aexp state = case aexp of
    Numeral n -> n
    Variable x -> getVar x state
    Bin op e1 e2 ->
        let
            v1 = evalAexp e1 state
            v2 = evalAexp e2 state
        in
            case op of
                Add -> v1 + v2
                Sub -> v1 - v2
                Mul -> v1 * v2

evalBexp :: Bexp -> State -> Bool
evalBexp bexp state = case bexp of
    Boolean b -> b
    Or b1 b2 -> evalBexp b1 state || evalBexp b2 state
    And b1 b2 -> evalBexp b1 state && evalBexp b2 state
    Not b -> not (evalBexp b state)
    Rel rop e1 e2 ->
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
    Skip -> return state
    Print e -> print (evalAexp e state) >> return state
    VarDef x e -> return $ setVar x (evalAexp e state) state
    Seq s1 s2 -> execStm s1 state >>= execStm s2
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
        state' <- execStm s local
        return $ setVar x old state'
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
        -- NOTE: Map.union is left-biased: on conflict vs1 (ps1) overrides vs2 (ps2)
        let vars' = Map.union vs1 vs2
        let procs' = Map.union ps1 ps2
        return (vars', procs')
    ProcDef name parameters returns body ->
        return $ setProc name (Proc parameters returns body) state
    ProcInvoc name arguments returns ->
        case Map.lookup name procs of
            Nothing -> do
                putStrLn $ "ERROR! procedure '" ++ name ++ "' undefined."
                return state
            Just (Proc params rets s) -> do
                let vals = map (`evalAexp` state) arguments -- evaluate arguments
                let local = (Map.fromList (zip params vals), procs) -- into local state
                state' <- execStm s local -- execute body
                let rets' = zip returns (map (`getVar` state') rets) -- extract returns
                return $ setVars rets' state -- insert into callside
                -- _ -> error $ "ERROR! unknown type of statement: " ++ show stm -- NOTE: development safeguard
