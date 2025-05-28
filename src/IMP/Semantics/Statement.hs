{- |
Module      : IMP.Semantics.Statement
Description : Execution semantics for statements in the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the execution semantics for statements in IMP.
It provides the 'execute' function, which interprets statements within a given
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

import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Pretty
import IMP.Result
import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Syntax

-- | Execute statement in state, returning new state in the REPL monad.
execute :: State -> Stm -> REPL State
execute state stm = case stm of
    Skip -> return state
    VarDef x f e -> do
        let v = getVar state x
        let v' = evaluate state e
        return $ case f of
            Def -> setVar state x v'
            Inc -> setVar state x (v + v')
            Dec -> setVar state x (v - v')
            Prod -> setVar state x (v * v')
            Quot -> setVar state x (v // v')
            Rem -> setVar state x (v %% v')
    Seq s1 s2 -> do
        state' <- execute state s1
        execute state' s2
    If b s1 s2 ->
        if evaluate state b
            then execute state s1
            else execute state s2
    While b s ->
        if evaluate state b
            then
                if not $ brk state
                    then do
                        state' <- execute state s
                        execute state' $ While b s
                    else return $ resetBreak state
            else return state
    Print e -> do
        display $ evaluate state e
        return state
    Read x -> do
        input <- inget $ x ++ " := "
        case readMaybe input of
            Just value -> return $ setVar state x value
            Nothing -> do
                display $ Info "invalid input, please enter an integer"
                execute state (Read x) -- retry reading input
    Local x e s -> do
        let old = getVar state x
        let new = evaluate state e
        let local = setVar state x new
        state' <- execute local s
        return $ setVar state' x old
    Par s1 s2 -> do
        -- {-# WARNING not actually parallel execution #-}
        state1 <- execute state s1
        state2 <- execute state s2
        -- union is left-biased: on conflict s1 dominates s2
        let vars' = Map.union (vrs state1) (vrs state2)
        let procs' = List.union (prs state1) (prs state2)
        return (vars', procs', brk state1)
    NonDet s1 s2 -> do
        left <- randomIO :: REPL Bool
        if left
            then execute state s1
            else execute state s2
    ProcDef p -> return $ setProc state p
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> throwError $ Error $ "undefined procedure: " ++ name
            Just (Proc _ (params, rets) body) -> do
                let vals = map (evaluate state) arguments -- evaluate arguments
                let local = (Map.fromList (zip params vals), prs state, brk state) -- into local state
                state' <- execute local body -- execute body
                let rets' = zip returns $ map (getVar state') rets -- extract returns
                return $ setVars state rets' -- insert into callside
    Break -> return $ setBreak state
    Revert s b -> do
        let old = state
        new <- execute state s
        if evaluate new b
            then return old
            else return new
    Match e ms d -> do
        let v = evaluate state e
        case lookup v ms of
            Just s -> execute state s
            Nothing -> execute state d
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
                state' <- execute state s1
                return $ setFlop state' i
            else do
                state' <- execute state s2
                return $ setFlip state' i
    Raise e -> throwError $ Raised $ evaluate state e
    Try s1 x s2 -> do
        state' <- catchError (execute state s1) $ \err -> case err of
            Raised v -> execute (setVar state x v) s2 -- catch in x, continue with s2
            _ -> throwError err -- can't catch, propagate
        return state'
    Swap x y -> do
        let v = getVar state x
        let w = getVar state y
        return $ setVar (setVar state x w) y v

-- | Execute first step in configuration, returning resulting configuration.
step :: State -> Stm -> REPL Conf
step state stm = case stm of
    Skip -> return (state, Nothing)
    VarDef _ _ _ -> do
        state' <- execute state stm
        return (state', Nothing)
    Seq s1 s2 -> do
        (state', rest) <- step state s1
        _ <- return $ case rest of
            Nothing -> (state', Nothing)
            Just s1' -> (state', Just $ Seq s1' s2)
        return (state, Just stm)
    If b s1 s2 ->
        if evaluate state b
            then return (state, Just s1)
            else return (state, Just s2)
    While b s ->
        if evaluate state b
            then return (state, Just $ Seq s stm)
            else return (state, Nothing)
    Print _ -> execute state stm >> return (state, Nothing)
    Read _ -> do
        state' <- execute state stm
        return (state', Nothing)
    Local _ _ _ -> undefined
    Par _ _ -> undefined
    NonDet _ _ -> undefined
    ProcDef _ -> do
        state' <- execute state stm
        return (state', Nothing)
    ProcInvoc _ _ -> undefined
    Break -> undefined
    Revert _ _ -> undefined
    Match e ms d -> do
        let v = evaluate state e
        case lookup v ms of
            Just s -> return $ (state, Just s)
            Nothing -> return $ (state, Just d)
    Havoc _ -> do
        state' <- execute state stm
        return (state', Nothing)
    Assert _ -> undefined
    Flip i s1 s2 ->
        if getFlip state i
            then return (setFlop state i, Just s1)
            else return (setFlip state i, Just s2)
    Raise _ -> undefined
    Try _ _ _ -> undefined
    Swap _ _ -> do
        state' <- execute state stm
        return (state', Nothing)

-- | Read line of input from the user with prompt, handling EOF.
inget :: String -> REPL String
inget prompt = do
    result <- liftIO $ do
        putStr prompt
        flush
        catch (Just <$> getLine) handleEOF
    case result of
        Just input -> return input
        Nothing -> do
            output ""
            output "Goodbye!"
            throwError Ok
    where
        handleEOF :: IOError -> IO (Maybe String)
        handleEOF _ = return Nothing

-- | Output string to the user, followed by a newline and flush.
output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

-- | Output value using its Show instance.
display :: (Show a) => a -> REPL ()
display = output . show

-- | Flush stdout.
flush :: IO ()
flush = hFlush stdout
