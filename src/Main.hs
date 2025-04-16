module Main where

import qualified Data.Map as Map
import Options.Applicative
import System.Exit (exitFailure)

import IMP.Parser (parseIMP)
import IMP.Exec (execStm)
import IMP.REPL (repl)

-- CLI actions
data Action = RunCommand String
    | RunFile FilePath
    | RunREPL

-- CLI parser
actionParser :: Parser Action
actionParser = RunCommand <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Run IMP command")
    <|> RunFile <$> argument str (metavar "FILE" <> help "Run IMP source file" )
    <|> pure RunREPL

actionInfo :: ParserInfo Action
actionInfo = info (actionParser <**> helper)
    (fullDesc <> progDesc "IMP language interpreter" <> header "imp - imperative language interpreter and REPL")

-- IMP CLI entrypoint
main :: IO ()
main = do
    action <- execParser actionInfo
    case action of
        RunREPL -> runREPL
        RunFile path -> runFile path
        RunCommand command -> runCommand command

runCommand :: String -> IO ()
runCommand command = case parseIMP "<command>" command of
    Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
    Right stm -> mapM_ putStrLn $ snd $ execStm stm Map.empty []

runFile :: FilePath -> IO ()
runFile path = do
    source <- readFile path
    case parseIMP path source of
        Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
        Right stm -> mapM_ putStrLn $ snd $ execStm stm Map.empty []

runREPL :: IO ()
runREPL = do
    putStrLn "Welcome to the IMP REPL! Type :quit to exit."
    repl Map.empty
