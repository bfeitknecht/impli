module Main where

import qualified Data.Map as Map
import Options.Applicative
import System.Exit (exitFailure)

import IMP.Parser (parseIMP)
import IMP.Exec (execStm)
import IMP.REPL (repl)

-- CLI actions
data Action = RunCode String
    | RunFile FilePath
    | RunREPL

-- CLI parser
actionParser :: Parser Action
actionParser = RunCode <$> strOption (long "code" <> short 'c' <> metavar "CODE" <> help "Run IMP code")
    <|> RunFile <$> argument str (metavar "FILE" <> help "Run IMP source file" )
    <|> pure RunREPL

actionInfo :: ParserInfo Action
actionInfo = info (actionParser <**> helper)
    (fullDesc <> progDesc "IMP language interpreter" <> header "imp - imperative toy language REPL and interpreter")

-- IMP CLI entrypoint
main :: IO ()
main = do
    action <- execParser actionInfo
    case action of
        RunREPL -> runREPL
        RunFile path -> runFile path
        RunCode code -> runCode code

runCode :: String -> IO ()
runCode code = case parseIMP code of
    Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
    Right stm -> mapM_ putStrLn $ snd $ execStm stm Map.empty []

runFile :: FilePath -> IO ()
runFile path = do
    code <- readFile path
    runCode code

runREPL :: IO ()
runREPL = do
    putStrLn "Welcome to the IMP REPL! Type :quit to exit."
    repl Map.empty
