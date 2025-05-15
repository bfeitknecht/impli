{- |
Module      : IMP.Semantics.Statement
Description : Execution semantics for statements in the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the execution semantics for statements in IMP.
It provides the `execute` function, which interprets statements within a given
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

execute :: State -> Stm -> REPL State
execute state stm = case stm of
    Skip -> return state
    VarDef x f e -> do
        let v = getVar state x
        let v' = evaluate state e
        return $ case f of
            Id -> setVar state x v'
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
        (vs1, ps1, flag') <- execute state s1
        (vs2, ps2, _) <- execute state s2
        -- {-# WARNING union is left-biased: on conflict *1 overrides *2 #-}
        let vars' = Map.union vs1 vs2
        let procs' = List.union ps1 ps2
        return (vars', procs', flag')
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
        state' <- catchError (execute state s1) handleRaised
        return state'
        where
            handleRaised err = case err of
                Raised v -> execute (setVar state x v) s2
                _ -> throwError err
    Swap x y -> do
        let vx = getVar state x
        let vy = getVar state y
        return $ setVar (setVar state x vy) y vx

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

output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

display :: (Show a) => a -> REPL ()
display = output . show

flush :: IO ()
flush = hFlush stdout
