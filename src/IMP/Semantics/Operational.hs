{- |
Module      : IMP.Semantics.Operational
Description : Operational semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of operational (small-step) semantics for the IMP language.
Allows execution of statement step by step to expose intermediate configurations.
-}
module IMP.Semantics.Operational (
    step,
    steps,
)
where

import Control.Monad.Except (catchError, throwError)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Execute first step of statement with state stack, return next configuration.
step :: (Stm, [State]) -> IMP Conf
step (_, []) = error "illegal configuration for step: empty state stack"
step (stm, stack@(state : states)) = case stm of
    Skip -> return (Nothing, stack)
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate a state
            state' = setVar state x $ case dop of
                Def -> v'
                Inc -> v + v'
                Dec -> v - v'
                Prod -> v * v'
                Quot -> v // v'
                Rem -> v %% v'
        in
            return (Nothing, state' : states)
    Seq s1 s2 -> do
        (rest, stack') <- step (s1, stack)
        case rest of
            Nothing -> return (Just s2, stack')
            Just s1' -> return (Just $ s1' <> s2, stack')
    IfElse b s1 s2 ->
        if evaluate b state
            then return (Just s1, stack)
            else return (Just s2, stack)
    While b s ->
        if evaluate b state
            then
                if not $ getBreak state
                    then return (Just $ s <> While b s, stack)
                    else return (Nothing, resetBreak state : states)
            else return (Nothing, stack)
    Print a -> display (evaluate a state) >> return (Nothing, stack)
    Read x -> do
        v <- getVal x
        return (Nothing, setVar state x v : states)
    Local x a s ->
        let
            snapshot = ([(x, getVar state x)], getProcs state, getBreak state)
            local = setVar state x $ evaluate a state
        in
            -- CHECK: perhaps push local state on stack and then pop later?
            return (Just $ s <> Restore snapshot, local : states)
    Par s1 s2 -> do
        left <- randomIO :: IMP Bool
        if left
            then do
                (rest1, stack') <- step (s1, stack)
                case rest1 of
                    Nothing -> return (Just s2, stack')
                    Just s1' -> return (Just $ Par s1' s2, stack')
            else do
                (rest2, stack') <- step (s2, stack)
                case rest2 of
                    Nothing -> return (Just s1, stack')
                    Just s2' -> return (Just $ Par s1 s2', stack')
    NonDet s1 s2 -> do
        left <- randomIO :: IMP Bool
        if left
            then return (Just s1, stack)
            else return (Just s2, stack)
    ProcDef p -> return (Nothing, setProc state p : states)
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> errata $ "undefined procedure: " ++ name
            Just (Procedure _ (params, rets) body)
                | length arguments /= length params -> errata "mismatched number of arguments to parameters"
                | length returns /= length rets -> errata "mismatched number of return variables"
                | otherwise ->
                    let
                        vals = map (`evaluate` state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state) -- into local state
                    in
                        return (Just $ body <> Return rets returns, local : stack)
    Restore (vars, procs, flag) ->
        let state' = setVars (getVars state, procs, flag) vars
        in return (Nothing, state' : states)
    Return rets returns -> case stack of
        (callee : caller : rest) ->
            let
                vals = map (getVar callee) rets
                caller' = setVars caller $ zip returns vals
            in
                return (Nothing, caller' : rest)
        _ -> error "illegal configuration for step: empty state stack"
    Break -> return (Nothing, setBreak state : states)
    Revert s b ->
        -- INFO: uninitialized variables can't be restored
        let snapshot = (Map.toList (getVars state), getProcs state, getBreak state)
        in return (Just $ s <> IfElse b (Restore snapshot) Skip, stack)
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> return (Just s, stack)
            Nothing -> return (Just d, stack)
    Havoc x -> do
        v <- randomIO :: IMP Integer
        return (Nothing, setVar state x v : states)
    Assert b ->
        if evaluate b state
            then return (Nothing, stack)
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then return (Just s1, setFlop state i : states)
            else return (Just s2, setFlip state i : states)
    Raise a -> throwError . Raised $ evaluate a state
    TryCatch s1 x s2 -> do
        (rest, stack') <- catchError (step (s1, stack)) $ \e -> case e of
            Raised v ->
                let state' = setVar state x v -- catch in x
                in return (Just s2, state' : states) -- continue with s2
            _ -> throwError e -- can't catch, propagate
        case rest of
            Nothing -> return (Nothing, stack')
            Just s1' -> return (Just $ TryCatch s1' x s2, stack')
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return (Nothing, setVars state [(x, w), (y, v)] : states)
    Timeout s a ->
        if evaluate a state <= 0
            then return (Nothing, stack)
            else do
                (rest, stack') <- step (s, stack)
                case rest of
                    Nothing -> return (Nothing, stack')
                    Just s' -> return (Just $ Timeout s' (a - 1), stack')
    Alternate s1 s2 -> do
        (rest, stack') <- step (s1, stack)
        case rest of
            Nothing -> return (Just s2, stack')
            Just s1' -> return (Just $ Alternate s2 s1', stack')

-- | Execute statement by repeated application of step until completion, return final state.
steps :: (Stm, [State]) -> IMP State
steps (_, []) = error "illegal configuration for steps: empty state stack"
steps conf = do
    (rest, stack'@(state' : _)) <- step conf
    case rest of
        Nothing -> return state'
        Just stm -> steps (stm, stack')
