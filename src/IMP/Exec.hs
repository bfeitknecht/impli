module IMP.Exec where

import Control.Concurrent.Async (concurrently)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Eval
import IMP.Syntax

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
    ProcDef name params rets body ->
        return $ setProc name (Proc params rets body) state
    ProcInvoc name args rets ->
        case Map.lookup name procs of
            Nothing -> error $ "Undefined procedure: " ++ name
            Just (Proc params rets' body) -> do
                -- evaluate arguments
                let vals = map (`evalAexp` state) args

                -- into local environment
                let vars' = Map.fromList (zip params vals)
                let local = (vars', procs)

                -- execute body
                (rets'', procs') <- execStm body local

                -- extract return values from local state
                let extracts = zip rets (map (\v -> Map.findWithDefault 0 v rets'') rets')

                -- insert into previous environment
                let fin = foldr (uncurry Map.insert) vars extracts
                return (fin, procs')
