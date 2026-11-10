{- |
Module      : IMP.Semantics.Natural
Description : Natural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of natural (big-step) semantics for the IMP language.
Execution is reified as a freer monad program over the 'Input', 'Output' and
'Choose' effects so the semantics itself stays pure: an interpreter discharges
the effects in whatever context the frontend supplies.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module IMP.Semantics.Natural (
    Input (..),
    Output (..),
    Choose (..),
    Effects,
    input,
    output,
    choose,
    fate,
    run,
) where

import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Freer.Error (Error, catchError, throwError)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Effect of reading an integer for a variable from the outside world.
data Input a where
    Input :: String -> Input Integer

-- | Effect of writing evaluation output to the outside world.
data Output a where
    Emit :: String -> Output ()

-- | Effect of an externally supplied arbitrary decision or value.
data Choose a where
    Pick :: Choose Bool -- ^ Pick a branch, 'True' for left.
    Fate :: Choose Integer -- ^ Produce an arbitrary integer.

-- | Canonical effect row for interpretation of 'run' programs.
type Effects = '[Input, Output, Choose, Error Exception]

-- | Read an integer for the named variable.
input :: (Member Input effs) => String -> Eff effs Integer
input x = send (Input x)

-- | Emit evaluation output.
output :: (Member Output effs) => String -> Eff effs ()
output text = send (Emit text)

-- | Pick a branch, 'True' for left.
choose :: (Member Choose effs) => Eff effs Bool
choose = send Pick

-- | Obtain an arbitrary integer.
fate :: (Member Choose effs) => Eff effs Integer
fate = send Fate

-- | Execute statement in the state, return final state.
run :: (Member Input effs, Member Output effs, Member Choose effs, Member (Error Exception) effs) => Gamma -> Eff effs State
run (stm, state) = case stm of
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
    Seq s1 s2 -> run (s1, state) >>= curry run s2
    IfElse b s1 s2 ->
        if evaluate b state
            then run (s1, state)
            else run (s2, state)
    While b s ->
        if evaluate b state
            then
                if not $ getBreak state
                    then run (s <> While b s, state)
                    else return $ resetBreak state
            else return state
    Print e -> output (show (evaluate e state)) >> return state
    Read x -> do
        v <- input x
        return $ setVar state x v
    Local x a s -> do
        let
            old = getVar state x
            new = evaluate a state
            local = setVar state x new
        state' <- run (s, local)
        return $ setVar state' x old
    Par s1 s2 -> do
        (vs1', ps1', flag') <- run (s1, state)
        (vs2', ps2', _) <- run (s2, state)
        -- INFO: On conflict, state result from s1 dominates
        return (Map.union vs1' vs2', Map.union ps1' ps2', flag')
    NonDet s1 s2 -> do
        left <- choose
        if left
            then run (s1, state)
            else run (s2, state)
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
                    state' <- run (body, local) -- run body
                    let rets' = zip returns $ map (getVar state') rets -- extract returns
                    return $ setVars state rets' -- insert into callside
    Restore _ -> throwError . Error $ "illegal statement for run in natural semantics: " ++ show stm
    Return {} -> throwError . Error $ "illegal statement for run in natural semantics: " ++ show stm
    Break -> return $ setBreak state
    Revert s b -> do
        let old = state
        new <- run (s, state)
        if evaluate b new
            then return old
            else return new
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> run (s, state)
            Nothing -> run (d, state)
    Havoc x -> do
        v <- fate
        return $ setVar state x v
    Assert b ->
        if evaluate b state
            then return state
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 -> do
        if getFlip state i
            then do
                state' <- run (s1, state)
                return $ setFlop state' i
            else do
                state' <- run (s2, state)
                return $ setFlip state' i
    Raise a -> throwError . Raised $ evaluate a state
    TryCatch s1 x s2 -> catchError (run (s1, state)) $ \e -> case e of
        Raised v -> run (s2, setVar state x v) -- catch in x, continue with s2
        _ -> throwError e -- can't catch, propagate
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError $ Error "timeout statement not supported in big-step semantics"
    Alternate _ _ -> throwError $ Error "alternate execution not supported in big-step semantics"
