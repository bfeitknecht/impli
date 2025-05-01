module IMP.REPL where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Parser
import IMP.Pretty
import IMP.Semantics
import IMP.Syntax

type Trace = [Stm]
type Env = (State, Trace)

st :: Env -> State
st = fst

tr :: Env -> Trace
tr = snd

repl :: State -> IO ()
repl state = runInputT defaultSettings (loop (state, []))

loop :: Env -> InputT IO ()
loop env = do
    line <- getInputLine "IMP> "
    case line of
        Nothing -> outputStrLn "Goodbye!" -- ctrl-d
        Just (':' : meta) -> handleMeta meta env
        Just input -> case parseInput "interactive" input of
            Left err -> do
                outputStrLn $ "ERROR! parse failure in: " ++ input
                outputStrLn $ show err
                loop env
            Right parsed -> do
                env' <- dispatch env parsed
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
        outputStrLn "All meta commands can be abbreviated by their first letter.\n"
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
    ["reset"] -> outputStrLn "INFO: state reset" >> loop (initial, [])
    ["trace"] -> do
        outputStrLn "Trace:"
        mapM_
            (\(i, s) -> outputStrLn $ "[" ++ show i ++ "]\t" ++ pretty s)
            (zip [1 :: Int ..] (tr env))
        loop env
    ["state"] -> do
        let (vars, procs) = st env
        outputStrLn "Variables:"
        mapM_ (\(k, v) -> outputStrLn $ "\t" ++ k ++ " = " ++ show v) (Map.toList vars)
        outputStrLn "Procedures:"
        mapM_ (\(k, p) -> outputStrLn $ "\t" ++ k ++ pretty p) (Map.toList procs)
        loop env
    ["load"] -> outputStrLn "INFO: no filepath provided" >> loop env
    ["load", path] -> do
        result <- liftIO $ readIMP (st env) path
        case result of
            Nothing -> loop env
            Just state -> loop (state, tr env)
    ["write"] -> outputStrLn "INFO: no filepath provided" >> loop env
    ["write", path] -> do
        let content = showTrace $ tr env
        void $ liftIO $ writeIMP path content
        loop env
    ("ast" : input) -> do
        if null input
            then outputStrLn "INFO: nothing to parse"
            else do void $ liftIO $ printAST (unwords input)
        loop env
    _ -> do
        outputStrLn $ "ERROR: not a meta command :" ++ meta ++ "\n"
        handleMeta "help" env

dispatch :: Env -> Construct -> InputT IO Env
dispatch env@(state, trace) construct = liftIO $ case construct of
    Statement s -> do
        state' <- execStm state s
        return (state', trace ++ [s])
    Arithm e -> print (evalAexp state e) >> return env
    Bool b -> putStrLn (if evalBexp state b then "true" else "false") >> return env
    Whitespace -> return env

printAST :: String -> IO Bool
printAST input = case parseInput "ast" input of
    Left err -> do
        putStrLn $ "ERROR! parse failure in: " ++ input
        print err
        return False
    Right parsed -> do
        print parsed
        return True

showTrace :: [Stm] -> String
showTrace [] = "skip\n"
showTrace stms = unlines $ map (++ ";") (init strs) ++ [last strs]
    where
        strs = map pretty stms

readIMP :: State -> FilePath -> IO (Maybe State)
readIMP state path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left err -> do
            putStrLn $ "ERROR! IO failure reading from file: " ++ path
            print err
            return Nothing
        Right content -> case parseProgram path content of
            Left err -> do
                putStrLn $ "ERROR! parse failure in: " ++ path
                print err
                return Nothing
            Right stm -> do
                state' <- execStm state stm
                putStrLn $ "INFO: interpreted file: " ++ path
                return $ Just state'

writeIMP :: FilePath -> String -> IO Bool
writeIMP path content = do
    result <- try (writeFile path content) :: IO (Either IOException ())
    case result of
        Left err -> do
            putStrLn $ "ERROR! IO failure writing to file: " ++ path
            print err
            return False
        Right () -> do
            putStrLn $ "INFO: trace written to: " ++ path
            return True
