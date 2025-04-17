module Main where

import Control.Exception (IOException, try)
import Control.Monad (void)
import qualified Data.Map as Map
import IMP.Exec (execStm)
import IMP.Parser (parseIMP)
import IMP.REPL (repl)
import Options.Applicative
import System.Exit (exitFailure)

-- IMP CLI entrypoint
main :: IO ()
main = do
    action <- execParser actionInfo
    case action of
        RunREPL -> runREPL
        RunSTDIN -> runSTDIN
        RunFile path -> runFile path
        RunCommand command -> runCommand command

-- CLI actions
data Action
    = RunCommand String
    | RunFile FilePath
    | RunSTDIN
    | RunREPL

-- CLI parser
actionParser :: Parser Action
actionParser =
    RunCommand
        <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Run IMP command")
            <|> flag' RunSTDIN (long "stdin" <> help "Read IMP program from standard input")
            <|> argument parseChannel (metavar "FILE" <> help "Run IMP source file (use '-' for stdin)")
            <|> pure RunREPL
    where
        parseChannel = eitherReader $ \arg -> if arg == "-" then Right RunSTDIN else Right (RunFile arg)

actionInfo :: ParserInfo Action
actionInfo =
    info
        (actionParser <**> helper)
        (fullDesc <> progDesc "IMP language interpreter" <> header "impli - imperative language interpreter and REPL")

runCommand :: String -> IO ()
runCommand command = case parseIMP "<command>" command of
    Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
    Right stm -> do
        _ <- execStm stm Map.empty
        return ()

runFile :: FilePath -> IO ()
runFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn $ "File not found: " ++ path
        Right content -> case parseIMP path content of
            Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
            Right stm -> void $ execStm stm Map.empty

runREPL :: IO ()
runREPL = do
    putStrLn "Welcome to the IMP REPL! Type :quit to exit."
    repl Map.empty

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    case parseIMP "<stdin>" input of
        Left err -> putStrLn $ "Parse error:\n" ++ show err
        Right stm -> void $ execStm stm Map.empty
