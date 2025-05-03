module IMP.REPL where

import Control.Exception (IOException, try)
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
                outputStrLn $ "ERROR: parse failure in: " ++ input
                outputStrLn $ show err
                loop env
            Right parsed -> do
                env' <- dispatch env parsed
                loop env'

normalizeMeta :: [String] -> [String]
normalizeMeta ws = case ws of
    ["?"] -> ["help"]
    ["h"] -> ["help"]
    ["q"] -> ["quit"]
    ["c"] -> ["clear"]
    ["r"] -> ["reset"]
    ["t"] -> ["trace"]
    ["s"] -> ["state"]
    (x : rest)
        | x == "l" || x == "load" -> ["load", unwords rest]
        | x == "w" || x == "write" -> ["write", unwords rest]
        | x == "a" || x == "ast" -> ["ast", unwords rest]
    _ -> ws

handleMeta :: String -> Env -> InputT IO ()
handleMeta meta env = case normalizeMeta (words meta) of
    [")"] -> outputStrLn "You look good today!" >> loop env
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
    ["clear"] -> liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0) >> loop env
    ["reset"] -> outputStrLn "INFO: state reset" >> loop (initial, [])
    ["trace"] -> do
        printSection
            "Trace:"
            [ "#" ++ show i ++ "\t" ++ l
            | (i, s) <- zip [1 :: Int ..] (tr env)
            , l <- lines (pretty s)
            ]
        loop env
    ["state"] -> do
        let (vars, procs) = st env
        printSection "Variables:" [k ++ " = " ++ show v | (k, v) <- Map.toList vars]
        printSection "Procedures:" [k ++ pretty p | (k, p) <- Map.toList procs]
        loop env
    ["load", path]
        | null path -> outputStrLn "INFO: no filepath provided" >> loop env
        | otherwise -> do
            result <- liftIO $ readIMP (st env) path
            case result of
                Nothing -> loop env
                Just state -> loop (state, tr env)
    ["write", path]
        | null path -> outputStrLn "INFO: no filepath provided" >> loop env
        | otherwise -> liftIO (writeIMP path $ showTrace $ tr env) >> loop env
    ["ast", input]
        | null input -> outputStrLn "INFO: nothing to parse" >> loop env
        | Just i <- parseIndex input
        , i <= length (tr env) ->
            liftIO (print $ tr env !! (i - 1)) >> loop env
        | input == "#" -> outputStrLn "INFO: no index provided" >> loop env
        | otherwise -> liftIO (printAST input) >> loop env
    _ -> do
        outputStrLn $ "ERROR: not a meta command :" ++ meta ++ "\n"
        handleMeta "help" env

dispatch :: Env -> Construct -> InputT IO Env
dispatch env@(state, trace) construct = liftIO $ case construct of
    Statement s -> do
        state' <- execStm state s
        return (state', trace ++ [s])
    Arithm e -> do
        putStrLn $ show (evalAexp state e)
        return env
    Bool b -> putStrLn (if evalBexp state b then "true" else "false") >> return env
    Whitespace -> return env

printAST :: String -> IO Bool
printAST input = case parseInput "ast" input of
    Left err -> do
        putStrLn $ "ERROR: parse failure in: " ++ input
        putStrLn $ show err
        return False
    Right parsed -> do
        putStrLn $ show parsed
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
            putStrLn $ "ERROR: IO failure reading from file: " ++ path
            putStrLn $ show err
            return Nothing
        Right content -> case parseProgram path content of
            Left err -> do
                putStrLn $ "ERROR: parse failure in: " ++ path
                putStrLn $ show err
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
            putStrLn $ "ERROR: IO failure writing to file: " ++ path
            putStrLn $ show err
            return False
        Right () -> do
            putStrLn $ "INFO: trace written to: " ++ path
            return True

parseIndex :: String -> Maybe Int
parseIndex ('#' : ds)
    | all (`elem` ['0' .. '9']) ds = Just (read ds)
parseIndex _ = Nothing

printSection :: String -> [String] -> InputT IO ()
printSection title section = case section of
    [] -> outputStrLn title
    _ -> outputStrLn $ title ++ "\n" ++ indent (unlines section)

indent :: String -> String
indent = unlines . map (replicate 2 ' ' ++) . lines -- prepend every line with two spaces
