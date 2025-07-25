{- |
Module      : IMP.Semantics.Statement
Description : Execution semantics for statements in the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the execution semantics for statements in IMP.
It provides the @interpret@ function, which interprets statements within a given
state and environment. The module supports a variety of imperative constructs,
including variable definitions, loops, conditionals, and procedure calls.
-}
module IMP.Semantics.Statement where

import Control.Exception (catch)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import System.Random (randomIO)
import Text.Read (readMaybe)

import qualified Data.Map as Map

import IMP.Config
import IMP.Pretty
import IMP.Result
import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Syntax

-- | Interpret statement in state, returning resulting state in REPL monad.
interpret :: State -> Stm -> REPL State
interpret state stm =
    if small
        then steps [state] stm
        else run state stm

-- | Run statement in state, returning resulting state in REPL monad.
run :: State -> Stm -> REPL State
run state stm = case stm of
    Skip -> return state
    VarDef x f e -> do
        let
            v = getVar state x
            v' = evaluate state e
        return $ setVar state x $ case f of
            Def -> v'
            Inc -> v + v'
            Dec -> v - v'
            Prod -> v * v'
            Quot -> v // v'
            Rem -> v %% v'
    Seq s1 s2 -> do
        state' <- run state s1
        run state' s2
    If b s1 s2 ->
        if evaluate state b
            then run state s1
            else run state s2
    While b s ->
        if evaluate state b
            then
                if not $ brk state
                    then run state $ Seq s $ While b s
                    else return $ resetBreak state
            else return state
    Print e -> do
        display $ evaluate state e
        return state
    Read x -> do
        input <- inget $ x ++ " := "
        case readMaybe input of
            Just v -> return $ setVar state x v
            Nothing -> do
                display $ Info "invalid input, please enter an integer"
                run state (Read x) -- retry reading input
    Local x e s -> do
        let
            old = getVar state x
            new = evaluate state e
            local = setVar state x new
        state' <- run local s
        return $ setVar state' x old
    Par _ _ -> throwError $ Error "parallel execution not supported in big-step semantics"
    NonDet s1 s2 -> do
        left <- randomIO :: REPL Bool
        if left
            then run state s1
            else run state s2
    ProcDef p -> return $ setProc state p
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> throwError $ Error $ "undefined procedure: " ++ name
            Just (Proc _ (params, rets) body)
                | length arguments /= length params -> throwError $ Error "mismatched parameters"
                | length returns /= length rets -> throwError $ Error "mismatched returns"
                | otherwise -> do
                    let
                        vals = map (evaluate state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), prs state, brk state) -- into local state
                    state' <- run local body -- run body
                    let rets' = zip returns $ map (getVar state') rets -- extract returns
                    return $ setVars state rets' -- insert into callside
    Break -> return $ setBreak state
    Revert s b -> do
        let old = state
        new <- run state s
        if evaluate new b
            then return old
            else return new
    Match e ms d -> do
        let v = evaluate state e
        case lookup v ms of
            Just s -> run state s
            Nothing -> run state d
    Havoc x -> do
        v <- randomIO :: REPL Integer
        return $ setVar state x v
    Assert b ->
        if evaluate state b
            then return state
            else throwError $ AssFail $ prettify b
    Flip i s1 s2 -> do
        if getFlip state i
            then do
                state' <- run state s1
                return $ setFlop state' i
            else do
                state' <- run state s2
                return $ setFlip state' i
    Raise e -> throwError $ Raised $ evaluate state e
    Try s1 x s2 -> catchError (run state s1) $ \err -> case err of
        Raised v -> run (setVar state x v) s2 -- catch in x, continue with s2
        _ -> throwError err -- can't catch, propagate
    Swap x y -> do
        let
            v = getVar state x
            w = getVar state y
        return $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError $ Error "timeout statement not supported in big-step semantics"
    Alternate _ _ -> throwError $ Error "alternate execution not supported in big-step semantics"
    _ -> error $ "illegal statement for big-step semantics: " ++ show stm

-- | Step in configuration, returning resulting configuration in REPL monad.
step :: [State] -> Stm -> REPL Conf
step [] _ = error "invalid configuration for step: empty state stack"
step stack@(state : states) stm = case stm of
    Skip -> return (stack, Nothing)
    VarDef x f e -> do
        let
            v = getVar state x
            v' = evaluate state e
            state' = setVar state x $ case f of
                Def -> v'
                Inc -> v + v'
                Dec -> v - v'
                Prod -> v * v'
                Quot -> v // v'
                Rem -> v %% v'
        return (state' : states, Nothing)
    Seq s1 s2 -> do
        (stack', rest) <- step stack s1
        case rest of
            Nothing -> return (stack', Just s2)
            Just s1' -> return (stack', Just $ Seq s1' s2)
    If b s1 s2 ->
        if evaluate state b
            then return (stack, Just s1)
            else return (stack, Just s2)
    While b s ->
        if evaluate state b
            then
                if not $ brk state
                    then return (stack, Just $ Seq s stm)
                    else return (resetBreak state : states, Nothing)
            else return (stack, Nothing)
    Print e -> do
        display (evaluate state e)
        return (stack, Nothing)
    Read x -> do
        input <- inget $ x ++ " := "
        case readMaybe input of
            Just v -> return (setVar state x v : states, Nothing)
            Nothing -> do
                display $ Info "invalid input, please enter an integer"
                step stack (Read x) -- retry reading input
    Local x e s -> do
        let
            snapshot = ([(x, getVar state x)], prs state, brk state)
            local = setVar state x $ evaluate state e
        return (local : states, Just $ Seq s $ Restore snapshot)
    Par s1 s2 -> do
        left <- randomIO :: REPL Bool
        if left
            then do
                (stack', rest1) <- step stack s1
                case rest1 of
                    Nothing -> return (stack', Just s2)
                    Just s1' -> return (stack', Just $ Par s1' s2)
            else do
                (stack', rest2) <- step stack s2
                case rest2 of
                    Nothing -> return (stack', Just s1)
                    Just s2' -> return (stack', Just $ Par s1 s2')
    NonDet s1 s2 -> do
        left <- randomIO :: REPL Bool
        if left
            then return (stack, Just s1)
            else return (stack, Just s2)
    ProcDef p -> return (setProc state p : states, Nothing)
    ProcInvoc name (arguments, returns) -> do
        case getProc state name of
            Nothing -> throwError $ Error $ "undefined procedure: " ++ name
            Just (Proc _ (params, rets) body)
                | length arguments /= length params -> throwError $ Error "mismatched number of arguments"
                | length returns /= length rets -> throwError $ Error "mismatched number of return values"
                | otherwise -> do
                    let
                        vals = map (evaluate state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), prs state, brk state) -- into local state
                    return (local : stack, Just $ Seq body $ Return rets returns)
    Restore (vars, procs, flag) -> do
        let state' = setVars (vrs state, procs, flag) vars
        return (state' : states, Nothing)
    Return rets returns -> case stack of
        (callee : caller : rest) -> do
            let
                vals = map (getVar callee) rets
                caller' = setVars caller $ zip returns vals
            return (caller' : rest, Nothing)
        _ -> throwError $ Error "insufficient callstack!"
    Break -> return (setBreak state : states, Nothing)
    Revert s b -> do
        -- uninitialized variables can't be restored!
        let snapshot = (Map.toList (vrs state), prs state, brk state)
        return (stack, Just $ Seq s $ If b (Restore snapshot) Skip)
    Match e ms d -> do
        let v = evaluate state e
        case lookup v ms of
            Just s -> return (stack, Just s)
            Nothing -> return (stack, Just d)
    Havoc x -> do
        v <- randomIO :: REPL Integer
        return (setVar state x v : states, Nothing)
    Assert b ->
        if evaluate state b
            then return (stack, Nothing)
            else throwError $ AssFail $ prettify b
    Flip i s1 s2 ->
        if getFlip state i
            then return (setFlop state i : states, Just s1)
            else return (setFlip state i : states, Just s2)
    Raise e -> throwError $ Raised $ evaluate state e
    Try s1 x s2 -> do
        (stack', rest) <- catchError (step stack s1) $ \err -> case err of
            Raised v -> do
                let state' = setVar state x v
                return (state' : states, Just s2)
            _ -> throwError err
        case rest of
            Nothing -> return (stack', Nothing)
            Just s1' -> return (stack', Just $ Try s1' x s2)
    Swap x y -> do
        let
            v = getVar state x
            w = getVar state y
        return (setVars state [(x, w), (y, v)] : states, Nothing)
    Timeout s e ->
        if evaluate state e <= 0
            then return (stack, Nothing)
            else do
                (stack', rest) <- step stack s
                case rest of
                    Nothing -> return (stack', Nothing)
                    Just s' -> return (stack', Just $ Timeout s' $ e - 1)
    Alternate s1 s2 -> do
        (stack', rest) <- step [state] s1
        case rest of
            Just s1' -> return (stack', Just $ Alternate s2 s1')
            Nothing -> return (stack', Just s2)

-- | Transition until terminal configuration is reached.
steps :: [State] -> Stm -> REPL State
steps stack stm = do
    (state' : states', rest) <- step stack stm
    case rest of
        Nothing -> return state'
        Just s' -> steps (state' : states') s'

-- | Read line of input from the user with prompt, handling EOF.
inget :: String -> REPL String
inget p = do
    result <- liftIO $ do
        putStr p
        flush
        catch (Just <$> getLine) handleEOF
    case result of
        Just input -> return input
        Nothing -> output "" >> output goodbye >> throwError Ok
    where
        handleEOF :: IOError -> IO (Maybe String)
        handleEOF _ = return Nothing

-- | Output string to the user, followed by newline and flush.
output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

-- | Display argument using its Show instance.
display :: (Show a) => a -> REPL ()
display = output . show

-- | Flush stdout.
flush :: IO ()
flush = hFlush stdout
