{- |
Module      : IMP.Semantic.Structural
Description : TODO
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.Semantic.Structural where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | TODO
run :: (Stm, State) -> IMP State
run (stm, state) = case stm of
    Skip -> return state
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate state a
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
        if evaluate state b
            then run (s1, state)
            else run (s2, state)
    While b s ->
        if evaluate state b
            then
                if not $ getBreak state
                    then run (s <> While b s, state)
                    else return $ resetBreak state
            else return state
    Print e -> liftIO (print $ evaluate state e) >> return state
    Read x -> do
        v <- getVal x
        return $ setVar state x v
    Local x a s -> do
        let
            old = getVar state x
            new = evaluate state a
            local = setVar state x new
        state' <- run (s, local)
        return $ setVar state' x old
    Par _ _ -> throwError . Error $ "parallel execution not (yet) supported in structural semantics" -- FIXME: this should be possible
    NonDet s1 s2 -> do
        left <- randomIO :: IMP Bool
        if left
            then run (s1, state)
            else run (s2, state)
    ProcDef p -> return $ setProc state p
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> throwError . Error $ "undefined procedure: " ++ name
            Just (Procedure _ (params, rets) body)
                | length arguments /= length params -> throwError . Error $ "mismatched number of arguments to parameters"
                | length returns /= length rets -> throwError . Error $ "mismatched number of return variables"
                | otherwise -> do
                    let
                        vals = map (evaluate state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state) -- into local state
                    state' <- run (body, local) -- run body
                    let rets' = zip returns $ map (getVar state') rets -- extract returns
                    return $ setVars state rets' -- insert into callside
    Restore _ -> error $ "illegal statement for run in structural semantics: " ++ show stm
    Return _ _ -> error $ "illegal statement for run in structural semantics: " ++ show stm
    Break -> return $ setBreak state
    Revert s b -> do
        let old = state
        new <- run (s, state)
        if evaluate new b
            then return old
            else return new
    Match a ms d ->
        let v = evaluate state a
        in case lookup v ms of
            Just s -> run (s, state)
            Nothing -> run (d, state)
    Havoc x -> do
        v <- randomIO :: IMP Integer
        return $ setVar state x v
    Assert b ->
        if evaluate state b
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
    Raise a -> throwError . Raised $ evaluate state a
    TryCatch s1 x s2 -> catchError (run (s1, state)) $ \e -> case e of
        Raised v -> run (s2, setVar state x v) -- catch in x, continue with s2
        _ -> throwError e -- can't catch, propagate
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError . Error $ "timeout statement not (yet) supported in big-step semantics"
    Alternate _ _ -> throwError . Error $ "alternate execution not supported in big-step semantics"
