module IMP.Semantics where

import Control.Concurrent.Async (concurrently)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Pretty
import IMP.Syntax

type Vars = Map.Map Var Integer
type Procs = Map.Map Var Proc
type State = (Vars, Procs)

initial :: State
initial = (Map.empty, Map.empty)

getVar :: State -> Var -> Integer
getVar (vars, _) x = Map.findWithDefault 0 x vars

setVar :: State -> Var -> Integer -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar (vars, procs) x v = (Map.insert x v vars, procs)

setVars :: State -> [(Var, Integer)] -> State
setVars = foldl (uncurry . setVar)

getProc :: State -> Var -> Maybe Proc
getProc (_, procs) name = Map.lookup name procs

setProc :: State -> Var -> Proc -> State
setProc (vars, procs) name def = (vars, Map.insert name def procs)

evalAexp :: State -> Aexp -> Val
evalAexp state aexp = case aexp of
    Numeral n -> n
    Variable x -> getVar state x
    Bin op e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case op of
                Add -> v1 + v2
                Sub -> v1 - v2
                Mul -> v1 * v2

evalBexp :: State -> Bexp -> Bool
evalBexp state bexp = case bexp of
    Boolean b -> b
    Or b1 b2 -> evalBexp state b1 || evalBexp state b2
    And b1 b2 -> evalBexp state b1 && evalBexp state b2
    Not b -> not (evalBexp state b)
    Rel rop e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case rop of
                Eq -> v1 == v2
                Neq -> v1 /= v2
                Lt -> v1 < v2
                Leq -> v1 <= v2
                Gt -> v1 > v2
                Geq -> v1 >= v2

execStm :: State -> Stm -> IO State
execStm state stm = case stm of
    Skip -> return state
    Print e -> print (evalAexp state e) >> return state
    VarDef x e -> return $ setVar state x $ evalAexp state e
    Seq s1 s2 -> do
        state' <- execStm state s1
        execStm state' s2
    If b s1 s2 ->
        if evalBexp state b
            then execStm state s1
            else execStm state s2
    While b s ->
        if evalBexp state b
            then execStm state $ Seq s $ While b s
            else return state
    Local x e s -> do
        let old = getVar state x
        let new = evalAexp state e
        let local = setVar state x new
        state' <- execStm local s
        return $ setVar state' x old
    NonDet s1 s2 -> do
        first <- (randomIO :: IO Bool)
        if first
            then execStm state s1
            else execStm state s2
    Par s1 s2 -> do
        ((vs1, ps1), (vs2, ps2)) <-
            concurrently
                (execStm state s1)
                (execStm state s2)
        -- NOTE: Map.union is left-biased: on conflict vs1 (ps1) overrides vs2 (ps2)
        let vars' = Map.union vs1 vs2
        let procs' = Map.union ps1 ps2
        return (vars', procs')
    ProcDef name (parameters, returns) body ->
        return $ setProc state name $ Proc (parameters, returns) body
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> do
                putStrLn $ "ERROR! undefined procedure: '" ++ name ++ "'"
                return state
            Just (Proc (params, rets) s) -> do
                let vals = map (evalAexp state) arguments -- evaluate arguments
                let local = (Map.fromList (zip params vals), snd state) -- into local state
                state' <- execStm local s -- execute body
                let rets' = zip returns $ map (getVar state') rets -- extract returns
                return $ setVars state rets' -- insert into callside
                -- _ -> error $ "ERROR! unknown type of statement: " ++ show stm -- NOTE: development safeguard
