{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : IMP.Semantics.Natural
Description : Natural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of natural (big-step) semantics for the IMP language.
Execution is abstracted over 'IMP.Effect.MonadIMP', which discharges input,
output and nondeterminism. The interleaving constructs @timeout@ and
@alternate@ have no big-step rules of their own and are delegated to the
structural semantics.
-}
module IMP.Semantics.Natural (
    run,
) where

import Control.Monad.Except (catchError, throwError)

import qualified Data.Map.Strict as Map

import IMP.Effect
import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

import qualified IMP.Semantics.Structural as Structural

-- | Execute statement in state to completion.
run :: (MonadIMP m) => Stm -> State -> m Flow
run stm state = case stm of
    Skip -> next state
    VarDef x dop a -> do
        v <- eval a state
        let u = getVar state x
        next . setVar state x $ case dop of
            Def -> v
            Inc -> u + v
            Dec -> u - v
            Prod -> u * v
            Quot -> u // v
            Rem -> u %% v
    Seq s1 s2 ->
        run s1 state >>= \case
            Next state' -> run s2 state'
            broken -> return broken
    IfElse b s1 s2 -> do
        v <- eval b state
        run (if v then s1 else s2) state
    While b s -> loop state
        where
            loop state' = do
                v <- eval b state'
                if v
                    then do
                        tick
                        run s state' >>= \case
                            Next state'' -> loop state''
                            Broken state'' -> next state''
                    else next state'
    Print a -> do
        v <- eval a state
        output (show v)
        next state
    Read x -> do
        v <- input x
        next (setVar state x v)
    Local x a s -> do
        v <- eval a state
        let old = lookupVar state x
        flow <- run s (setVar state x v)
        return $ onFlow (\state' -> restoreVar state' x old) flow
    Par s1 s2 -> do
        flow1 <- run s1 state
        flow2 <- run s2 state
        -- INFO: On conflict, state result from s1 dominates
        let
            State vs1 ps1 = flowState flow1
            State vs2 ps2 = flowState flow2
            union = State (Map.union vs1 vs2) (Map.union ps1 ps2)
        return $ case (flow1, flow2) of
            (Next _, Next _) -> Next union
            _ -> Broken union
    NonDet s1 s2 -> do
        left <- choose
        run (if left then s1 else s2) state
    ProcDef name params rets body -> next $ setProc state name (params, rets, body)
    ProcInvoc name arguments returns -> case getProc state name of
        Nothing -> throwError . Error $ "undefined procedure: " ++ name
        Just (params, rets, body)
            | length arguments /= length params -> throwError $ Error "mismatched number of arguments to parameters"
            | length returns /= length rets -> throwError $ Error "mismatched number of return variables"
            | otherwise -> do
                vals <- mapM (`eval` state) arguments
                tick
                run body (State (Map.fromList (zip params vals)) (procs state)) >>= \case
                    Next local -> next . setVars state $ zip returns (map (getVar local) rets)
                    Broken _ -> throwError $ Error "break outside loop"
    Break -> return (Broken state)
    Revert s b -> do
        flow <- run s state
        v <- eval b (flowState flow)
        return $ if v then onFlow (const state) flow else flow
    Match a ms d -> do
        v <- eval a state
        run (maybe d id (lookup v ms)) state
    Havoc x -> do
        v <- arbitrary
        next (setVar state x v)
    Assert b -> do
        v <- eval b state
        if v
            then next state
            else throwError . AssertFail $ stringify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then run s1 (setFlop state i)
            else run s2 (setFlip state i)
    Raise a -> eval a state >>= throwError . Raised
    TryCatch s1 x s2 -> catchError (run s1 state) $ \case
        Raised v -> run s2 (setVar state x v) -- catch in x, continue with s2
        e -> throwError e -- can't catch, propagate
    Swap x y -> next $ setVars state [(x, getVar state y), (y, getVar state x)]
    Timeout {} -> Structural.run stm state
    Alternate {} -> Structural.run stm state
{-# INLINEABLE run #-}

-- | Complete normally.
next :: (Monad m) => State -> m Flow
next = return . Next

-- | Evaluate expression in the interpretation monad.
eval :: (MonadIMP m, Evaluate a b) => a -> State -> m b
eval e state = liftEval (evaluate e state)
