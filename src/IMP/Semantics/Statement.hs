module IMP.Semantics.Statement where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (concurrently)
import Control.Exception (catch, throwIO)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import System.Random (randomIO)
import Text.Read (readMaybe)

import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Syntax.Types

class Executes a where
    executes :: State -> a -> REPL State

instance Executes Stm where
    executes state stm = case stm of
        Skip -> return state
        VarDef x f e -> do
            let v = getVar state x
            let v' = evaluates state e
            pure $ case f of
                Id -> setVar state x v'
                Inc -> setVar state x (v + v')
                Dec -> setVar state x (v - v')
                Prod -> setVar state x (v * v')
        Seq s1 s2 -> do
            state' <- executes state s1
            executes state' s2
        If b s1 s2 -> if evaluates state b
            then executes state s1
            else executes state s2
        While b s -> if evaluates state b
            then
                if not $ brk state
                    then do
                        state' <- executes state s
                        executes state' $ While b s
                    else return $ resetBreak state
            else return state
        Print e -> do
            liftIO $ print (evaluates state e)
            return state
        _ -> return state

execStm :: State -> Stm -> IO State
execStm state stm =
    case stm of
        Skip -> return state
        VarDef x f e -> do
            let v = getVar state x
            let v' = evalAexp state e
            return $ case f of
                Id -> setVar state x v'
                Inc -> setVar state x (v + v')
                Dec -> setVar state x (v - v')
                Prod -> setVar state x (v * v')
        Seq s1 s2 -> do
            state' <- execStm state s1
            execStm state' s2
        If b s1 s2 ->
            if evalBexp state b
                then execStm state s1
                else execStm state s2
        While b s -> case evalBexp state b of
            True
                | brk state -> return $ resetBreak state
                | otherwise -> do
                    state' <- execStm state s
                    execStm state' $ While b s
            _ -> return state
        Print e -> print (evalAexp state e) >> return state
        Read x -> do
            result <- readInput $ x ++ " := "
            case result of
                Nothing -> do
                    putStrLn ""
                    exitSuccess
                Just input ->
                    case readMaybe input of
                        Just value -> return $ setVar state x value
                        Nothing -> do
                            putStrLn "*** ERROR: invalid input, please enter an integer!"
                            execStm state (Read x) -- retry reading input
        Local x e s -> do
            let old = getVar state x
            let new = evalAexp state e
            let local = setVar state x new
            state' <- execStm local s
            return $ setVar state' x old
        Par s1 s2 -> do
            ((vs1, ps1, flag'), (vs2, ps2, _)) <-
                concurrently
                    (execStm state s1)
                    (execStm state s2)
            -- NOTE: union is left-biased: on conflict vs1 (ps1) overrides vs2 (ps2)
            let vars' = Map.union vs1 vs2
            let procs' = List.union ps1 ps2
            return (vars', procs', flag')
        NonDet s1 s2 -> do
            left <- randomIO :: IO Bool
            if left
                then execStm state s1
                else execStm state s2
        ProcDef p -> return $ setProc state p
        ProcInvoc name (arguments, returns) ->
            case getProc state name of
                Nothing -> do
                    putStrLn $ "*** ERROR: undefined procedure: " ++ name
                    return state
                Just (Proc _ (params, rets) body) -> do
                    let vals = map (evalAexp state) arguments -- evaluate arguments
                    let local = (Map.fromList (zip params vals), prs state, brk state) -- into local state
                    state' <- execStm local body -- execute body
                    let rets' = zip returns $ map (getVar state') rets -- extract returns
                    return $ setVars state rets' -- insert into callside
        Break -> return $ setBreak state
        Revert s b -> do
            let old = state
            new <- execStm state s
            if evalBexp new b
                then return old
                else return new
        Match e ms d -> do
            let v = evalAexp state e
            case lookup v ms of
                Just s -> execStm state s
                Nothing -> execStm state d
        Havoc x -> do
            randomValue <- randomIO :: IO Integer
            return $ setVar state x randomValue
        Assert b ->
            if evalBexp state b
                then return state
                else do
                    putStrLn $ "*** ERROR: assertion failure in: " ++ show b
                    exitFailure
        Flip i s1 s2 -> do
            if getVar state ("_" ++ show i) == 0
                then do
                    state' <- execStm state s1
                    return $ setVar state' ("_" ++ show i) 1
                else do
                    state' <- execStm state s2
                    return $ setVar state' ("_" ++ show i) 0
        Raise e -> do
            let v = evalAexp state e
            throwIO (Throw v)
        Try s1 x s2 -> catch (execStm state s1) $
            \(Throw v) -> do
                let state' = setVar state x v
                execStm state' s2

readInput :: String -> IO (Maybe String)
readInput prompt = do
    putStr prompt
    hFlush stdout
    catch (Just <$> getLine) handleEOF
    where
        handleEOF :: IOError -> IO (Maybe String)
        handleEOF _ = return Nothing
