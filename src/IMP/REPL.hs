module IMP.REPL (repl) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

import qualified Data.Map as Map

import IMP.Eval (State)
import IMP.Exec (execStm)
import IMP.Parser (parseIMP)

repl :: State -> IO ()
repl state = runInputT defaultSettings (loop state)

loop :: State -> InputT IO ()
loop state = do
    input <- getInputLine "IMP> "
    case input of
        Nothing -> outputStrLn "Goodbye!" -- ctrl-D
        Just (':' : meta) -> handleMeta meta state
        Just "" -> loop state
        Just line -> case parseIMP "<interactive>" line of
            Left err -> do
                outputStrLn $ "Parse error: " ++ show err
                loop state
            Right stm -> do
                -- lift from InputT to IO
                state' <- liftIO $ execStm stm state
                loop state'

handleMeta :: String -> State -> InputT IO ()
handleMeta meta state = case words meta of
    ["q"] -> outputStrLn "Goodbye!"
    ["r"] -> handleMeta "reset" state
    ["h"] -> handleMeta "help" state
    ["?"] -> handleMeta "help" state
    ["l"] -> handleMeta "load" state
    ["l", path] -> handleMeta ("load " ++ path) state
    ["quit"] -> outputStrLn "Goodbye!"
    ["reset"] -> do
        outputStrLn "State reset."
        loop Map.empty
    ["help"] -> do
        outputStrLn "All meta commands can be abbreviated by their first letter."
        outputStrLn ":quit         Quit the REPL"
        outputStrLn ":reset        Reset state"
        outputStrLn ":help / :?    Show this help"
        -- outputStrLn ":env          Show environment"
        outputStrLn ":load FILE    Evaluate a file and load the corresponding state"
        outputStrLn ""
        loop state
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
                case parseIMP path content of
                    Left err -> do
                        outputStrLn $ "Parse error in file: " ++ path ++ "\n" ++ show err
                        loop state
                    Right stm -> do
                        state' <- liftIO $ execStm stm state
                        loop state'
    _ -> do
        outputStrLn $ "Not a meta command. :" ++ meta
        handleMeta "help" state
