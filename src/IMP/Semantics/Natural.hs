{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : IMP.Semantics.Natural
Description : Natural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of natural (big-step) semantics for the IMP language.
Execution is abstracted over an arbitrary 'MonadError' monad and reads or
writes the outside world only through the supplied 'Handlers', so the
semantics itself stays pure: a frontend discharges the handlers in whatever
context it runs in.
-}
module IMP.Semantics.Natural (
    run,
) where

import Control.Monad.Except (MonadError, catchError, throwError)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Handlers
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Execute statement in the state with effects discharged by handlers, return final state.
run :: (MonadError Exception m) => Handlers m -> Gamma -> m State
run handlers (stm, state) = case stm of
    Skip -> return state
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate a state
        in
            return $ setVar state x $ case dop of
                Def -> v'
                Inc -> v + v'
                Dec -> v - v'
                Prod -> v * v'
                Quot -> v // v'
                Rem -> v %% v'
    Seq s1 s2 -> run handlers (s1, state) >>= curry (run handlers) s2
    IfElse b s1 s2 ->
        if evaluate b state
            then run handlers (s1, state)
            else run handlers (s2, state)
    While b s ->
        if evaluate b state
            then
                if not $ getBreak state
                    then run handlers (s <> While b s, state)
                    else return $ resetBreak state
            else return state
    Print e -> onOutput handlers (show (evaluate e state)) >> return state
    Read x -> do
        v <- onInput handlers x
        return $ setVar state x v
    Local x a s -> do
        let
            old = getVar state x
            new = evaluate a state
            local = setVar state x new
        state' <- run handlers (s, local)
        return $ setVar state' x old
    Par s1 s2 -> do
        (vs1', ps1', flag') <- run handlers (s1, state)
        (vs2', ps2', _) <- run handlers (s2, state)
        -- INFO: On conflict, state result from s1 dominates
        return (Map.union vs1' vs2', Map.union ps1' ps2', flag')
    NonDet s1 s2 -> do
        left <- onChoice handlers
        if left
            then run handlers (s1, state)
            else run handlers (s2, state)
    ProcDef name params rets body -> return $ setProc state name (params, rets, body)
    ProcInvoc name arguments returns ->
        case getProc state name of
            Nothing -> throwError . Error $ "undefined procedure: " ++ name
            Just (params, rets, body)
                | length arguments /= length params -> throwError $ Error "mismatched number of arguments to parameters"
                | length returns /= length rets -> throwError $ Error "mismatched number of return variables"
                | otherwise -> do
                    let
                        vals = map (`evaluate` state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state) -- into local state
                    state' <- run handlers (body, local) -- run body
                    let rets' = zip returns $ map (getVar state') rets -- extract returns
                    return $ setVars state rets' -- insert into callside
    Restore _ -> throwError . Error $ "illegal statement for run in natural semantics: " ++ show stm
    Return {} -> throwError . Error $ "illegal statement for run in natural semantics: " ++ show stm
    Break -> return $ setBreak state
    Revert s b -> do
        let old = state
        new <- run handlers (s, state)
        if evaluate b new
            then return old
            else return new
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> run handlers (s, state)
            Nothing -> run handlers (d, state)
    Havoc x -> do
        v <- onRandom handlers
        return $ setVar state x v
    Assert b ->
        if evaluate b state
            then return state
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 -> do
        if getFlip state i
            then do
                state' <- run handlers (s1, state)
                return $ setFlop state' i
            else do
                state' <- run handlers (s2, state)
                return $ setFlip state' i
    Raise a -> throwError . Raised $ evaluate a state
    TryCatch s1 x s2 -> catchError (run handlers (s1, state)) $ \e -> case e of
        Raised v -> run handlers (s2, setVar state x v) -- catch in x, continue with s2
        _ -> throwError e -- can't catch, propagate
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError $ Error "timeout statement not supported in big-step semantics"
    Alternate _ _ -> throwError $ Error "alternate execution not supported in big-step semantics"
