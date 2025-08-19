{-# LANGUAGE ScopedTypeVariables #-}

module IMP.Semantic.Operational where

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)

import qualified Data.Map as Map

step :: (Stm, [State]) -> IMP Conf
step (_, []) = error "illegal configuration for step: empty state stack"
step (stm, stack@(state : states)) = case stm of
    Skip -> return (Nothing, stack)
    VarDef x f a ->
        let
            v = getVar state x
            v' = evaluate state a
            state' = setVar state x $ case f of
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
            Just s1' -> return (Just $ Seq s1' s2, stack')
    IfElse b s1 s2 ->
        if evaluate state b
            then return (Just s1, stack)
            else return (Just s2, stack)
    While b s ->
        if evaluate state b
            then
                if not $ getBreak state
                    then return (Just $ Seq s $ While b s, stack)
                    else return (Nothing, resetBreak state : states)
            else return (Nothing, stack)
    Print a -> liftIO (print $ evaluate state a) >> return (Nothing, stack)
    Read x -> do
        v <- getVal x
        return (Nothing, setVar state x v : states)
    Local x a s ->
        let
            snapshot = ([(x, getVar state x)], getProcs state, getBreak state)
            local = setVar state x $ evaluate state a
        in
            -- maybe put local on stack and make Restore simply Pop the top state?
            return (Just $ Seq s $ Restore snapshot, local : states)
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
            Nothing -> throwError . Error $ "undefined procedure: " ++ name
            Just (Procedure _ (params, rets) body)
                | length arguments /= length params -> throwError . Error $ "mismatched number of arguments to parameters"
                | length returns /= length rets -> throwError . Error $ "mismatched number of return variables"
                | otherwise ->
                    let
                        vals = map (evaluate state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state) -- into local state
                    in
                        return (Just $ Seq body $ Return rets returns, local : stack)
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
        -- INFO: uninitialized variables can't be restored!
        let snapshot = (Map.toList (getVars state), getProcs state, getBreak state)
        in return (Just $ Seq s $ IfElse b (Restore snapshot) Skip, stack)
    Match a ms d ->
        let v = evaluate state a
        in case lookup v ms of
            Just s -> return (Just s, stack)
            Nothing -> return (Just d, stack)
    Havoc x -> do
        v <- randomIO :: IMP Integer
        return (Nothing, setVar state x v : states)
    Assert b ->
        if evaluate state b
            then return (Nothing, stack)
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then return (Just s1, setFlop state i : states)
            else return (Just s2, setFlip state i : states)
    Raise a -> throwError . Raised $ evaluate state a
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
        if evaluate state a <= 0
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

steps :: (Stm, [State]) -> IMP State
steps (_, []) = error "insufficient"
steps conf = do
    (rest, stack'@(state' : _)) <- step conf
    case rest of
        Nothing -> return state'
        Just stm -> steps (stm, stack')
