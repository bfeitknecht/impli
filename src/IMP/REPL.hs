module IMP.REPL where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import System.Console.Haskeline

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Eval
import IMP.Exec
import IMP.Parser
import IMP.Syntax

repl :: State -> IO ()
repl state = runInputT defaultSettings (loop state)

loop :: State -> InputT IO ()
loop state = do
    input <- getInputLine "IMP> "
    case input of
        Nothing -> outputStrLn "Goodbye!" -- ctrl-D
        Just (':' : meta) -> handleMeta meta state
        Just "" -> loop state
        Just input -> case parseInput "<interactive>" input of
            Left err -> do
                outputStrLn $ "Parse error: " ++ show err
                loop state
            Right parsed -> do
                state' <- dispatch parsed state
                loop state'

handleMeta :: String -> State -> InputT IO ()
handleMeta meta state = case words meta of
    [")"] -> outputStrLn "You look good today!" >> loop state
    ["("] -> outputStrLn "Hang in there, it'll be okay." >> loop state
    ["?"] -> handleMeta "help" state
    ["h"] -> handleMeta "help" state
    ["q"] -> outputStrLn "Goodbye!"
    ["c"] -> handleMeta "clear" state
    ["r"] -> handleMeta "reset" state
    ["l"] -> handleMeta "load" state
    ["l", path] -> handleMeta ("load " ++ path) state
    ("a" : input) -> handleMeta ("ast " ++ (unwords input)) state
    ["help"] -> do
        outputStrLn "All meta commands can be abbreviated by their first letter."
        outputStrLn ":help / :?    Show this help"
        outputStrLn ":quit         Quit the REPL"
        outputStrLn ":clear        Clear the screen"
        outputStrLn ":reset        Reset state"
        -- outputStrLn ":env          Show the current environment"
        outputStrLn ":load FILE    Evaluate a file and load the corresponding state"
        outputStrLn ":ast INPUT    Parse input and display the resulting AST"
        outputStrLn ""
        loop state
    ["quit"] -> outputStrLn "Goodbye!"
    ["clear"] -> do
        liftIO $ ANSI.clearScreen >> ANSI.setCursorPosition 0 0
        loop state
    ["reset"] -> do
        outputStrLn "State reset."
        loop Map.empty
    ["load"] -> do
        outputStrLn "No filepath provided."
        loop state
    ["load", path] -> do
        result <- liftIO $ try (readFile path) :: InputT IO (Either IOException String)
        case result of
            Left _ -> do
                outputStrLn $ "File not found: " ++ path
                loop state
            Right content ->
                case parseProgram path content of
                    Left err -> do
                        outputStrLn $ "Parse error in file: " ++ path ++ "\n" ++ show err
                        loop state
                    Right stm -> do
                        state' <- liftIO $ execStm stm state
                        loop state'
    ("ast" : input) -> do
        if null input
            then outputStrLn "No statement to parse."
            else printAST (unwords input)
        loop state
    _ -> do
        outputStrLn $ "Not a meta command. :" ++ meta
        handleMeta "help" state

dispatch :: Construct -> State -> InputT IO State
dispatch (Statement stm) state = liftIO (execStm stm state)
dispatch (Arithm e) state = outputStrLn (show (evalAexp e state)) >> return state
dispatch (Bool b) state =
    let str = show (evalBexp b state)
    in outputStrLn (toLower (head str) : tail str) >> return state

printAST :: String -> InputT IO ()
printAST input = case parseInput "<ast>" input of
    Left err -> do outputStrLn $ "Parse error: " ++ show err
    Right construct -> do outputStrLn $ pretty construct

pretty :: Construct -> String -- replace with actual pretty printer someday
pretty construct = case construct of
    Statement stm -> show stm
    Arithm e -> show e
    Bool b -> show b
