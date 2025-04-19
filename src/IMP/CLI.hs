module IMP.CLI where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import qualified Data.Map as Map

import IMP.Exec
import IMP.Parser
import IMP.REPL

import qualified Paths_impli as Paths

data Action
    = RunCommand String
    | RunFile FilePath
    | RunSTDIN
    | RunREPL
    | PrintAST String

actionParser :: Parser Action
actionParser =
    RunCommand
        <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Run IMP command")
            <|> PrintAST
        <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Print the AST of the given construct")
            <|> RunFile
        <$> strArgument (metavar "FILE" <> help "Run IMP source file (use '-' for stdin)")
            <|> flag' RunSTDIN (long "stdin" <> help "Read IMP program from standard input")
            <|> pure RunREPL

actionInfo :: ParserInfo Action
actionInfo =
    info
        (actionParser <**> helper <**> printVersion)
        ( fullDesc
            <> progDesc "An interpreter and REPL for the imperative toy language IMP"
            <> header "impli - The IMP language interpreter"
        )

runCommand :: String -> IO ()
runCommand command = case parseProgram "<command>" command of
    Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
    Right stm -> void $ execStm stm Map.empty

runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn $ "File not found: " ++ path
        Right content -> case parseProgram path content of
            Left err -> putStrLn ("Parse error:\n" ++ show err) >> exitFailure
            Right stm -> void $ execStm stm Map.empty

runREPL :: IO ()
runREPL = do
    putStrLn "Welcome to the IMP REPL! Type :quit to exit."
    repl Map.empty

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    case parseProgram "<stdin>" input of
        Left err -> putStrLn $ "Parse error:\n" ++ show err
        Right stm -> void $ execStm stm Map.empty

versionString :: String
versionString = "impli " ++ showVersion Paths.version

printVersion :: Parser (a -> a)
printVersion =
    infoOption
        versionString
        ( long "version"
            <> short 'v'
            <> help "Show version"
        )
