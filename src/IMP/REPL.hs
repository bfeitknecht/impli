module IMP.REPL where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import System.Console.Haskeline

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Parser
import IMP.Pretty
import IMP.Semantics
import IMP.Syntax

type Trace = [Stm]
type Env = (State, Trace)

state :: Env -> State
state = fst
trace :: Env -> Trace
trace = snd

repl :: State -> IO ()
repl state = runInputT defaultSettings (loop (state, []))

loop :: Env -> InputT IO ()
loop env = do
    line <- getInputLine "IMP> "
    case line of
        Nothing -> outputStrLn "Goodbye!" -- ctrl-d
        Just (':' : meta) -> handleMeta meta env
        Just "" -> loop env
        Just input -> case parseInput "interactive" input of
            Left err -> do
                outputStrLn $ "ERROR! no parse: " ++ input ++ "\n" ++ show err
                loop env
            Right parsed -> do
                env' <- dispatch parsed env
                loop env'

handleMeta :: String -> Env -> InputT IO ()
handleMeta meta env = case words meta of
    [")"] -> outputStrLn "You look good today!" >> loop env
    ["?"] -> handleMeta "help" env
    ["h"] -> handleMeta "help" env
    ["q"] -> outputStrLn "Goodbye!"
    ["c"] -> handleMeta "clear" env
    ["r"] -> handleMeta "reset" env
    ["t"] -> handleMeta "trace" env
    ["s"] -> handleMeta "state" env
    ["l"] -> handleMeta "load" env
    ["l", path] -> handleMeta ("load " ++ path) env
    ["w"] -> handleMeta "write" env
    ["w", path] -> handleMeta ("write " ++ path) env
    ("a" : input) -> handleMeta ("ast " ++ (unwords input)) env
    ["help"] -> do
        outputStrLn "All meta commands can be abbreviated by their first letter."
        outputStrLn ":help / :?    Show this help"
        outputStrLn ":quit         Quit REPL"
        outputStrLn ":clear        Clear screen"
        outputStrLn ":reset        Reset state"
        outputStrLn ":trace        Show trace (executed statements)"
        outputStrLn ":state        Show state (defined variables and procedures)"
        outputStrLn ":load FILE    Interpret file and load resulting state"
        outputStrLn ":write FILE   Write trace to file (relative to CWD)"
        outputStrLn ":ast INPUT    Parse input and display corresponding AST"
        outputStrLn ""
        loop env
    ["quit"] -> outputStrLn "Goodbye!"
    ["clear"] -> do
        liftIO $ ANSI.clearScreen >> ANSI.setCursorPosition 0 0
        loop env
    ["reset"] -> outputStrLn "INFO: state reset." >> loop (emptyState, [])
    ["trace"] -> do
        outputStrLn "Trace:"
        mapM_
            (\(i, s) -> outputStrLn $ "[" ++ show i ++ "]\t" ++ pretty s)
            (zip [1 ..] (trace env))
        loop env
    ["state"] -> do
        let (vars, procs) = state env
        outputStrLn "Variables:"
        mapM_ (\(k, v) -> outputStrLn $ "\t" ++ k ++ " = " ++ show v) (Map.toList vars)
        outputStrLn "Procedures:"
        mapM_ (\(k, p) -> outputStrLn $ "\t" ++ k ++ show p) (Map.toList procs)
        loop env
    ["load"] -> outputStrLn "INFO: no filepath provided" >> loop env
    ["load", path] -> do
        result <- liftIO $ try (readFile path) :: InputT IO (Either IOException String)
        case result of
            Left _ -> (outputStrLn $ "INFO: file not found: " ++ path) >> loop env
            Right content ->
                case parseProgram path content of
                    Left err -> do
                        outputStrLn $ "ERROR! no parse in file: " ++ path ++ "\n" ++ show err
                        loop env
                    Right stm -> do
                        state' <- liftIO $ execStm stm $ state env
                        loop (state', trace env)
    ["write"] -> outputStrLn "INFO: no filepath provided" >> loop env
    ["write", path] -> outputStrLn "TODO!" >> loop env
    ("ast" : input) -> do
        if null input
            then outputStrLn "INFO: nothing to parse."
            else printAST (unwords input)
        loop env
    _ -> do
        outputStrLn $ "INFO: not a meta command. :" ++ meta ++ "\n"
        handleMeta "help" env

dispatch :: Construct -> Env -> InputT IO Env
dispatch construct env@(st, tr) = case construct of
    Statement s -> do
        st' <- liftIO $ execStm s st
        return (st', tr ++ [s])
    Arithm e -> outputStrLn (show $ evalAexp e st) >> return env
    Bool b -> outputStrLn (map toLower $ show $ evalBexp b st) >> return env

printAST :: String -> InputT IO ()
printAST input = case parseInput "ast" input of
    Left err -> do outputStrLn $ "ERROR! no parse: " ++ input ++ "\n" ++ show err
    Right construct -> do outputStrLn $ show construct
