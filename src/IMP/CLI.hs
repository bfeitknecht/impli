module IMP.CLI where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import IMP.Parser
import IMP.REPL
import IMP.Semantics

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
        <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
            <|> PrintAST
        <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Print AST")
            <|> RunFile
        <$> strArgument (metavar "FILE" <> help "Interpret source file")
            <|> flag' RunSTDIN (long "stdin" <> help "Interpret from standard input")
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
runCommand command = case parseProgram "command" command of
    Left err -> putStrLn ("ERROR! no parse: " ++ command ++ "\n" ++ show err) >> exitFailure
    Right stm -> void $ execStm stm emptyState

runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> do
            putStrLn $ "ERROR! file not found: " ++ path
        Right content -> case parseProgram "interpreter" content of
            Left err -> putStrLn ("ERROR! no parse in file: " ++ path ++ "\n" ++ show err) >> exitFailure
            Right stm -> void $ execStm stm emptyState

runREPL :: IO ()
runREPL = do
    putStrLn "Welcome to the IMP REPL! Type :quit to exit."
    repl emptyState

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    case parseProgram "stdin" input of
        Left err -> putStrLn $ "ERROR! no parse:" ++ input ++ "\n" ++ show err
        Right stm -> void $ execStm stm emptyState

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
