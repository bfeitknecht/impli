module IMP.CLI where

import Control.Monad (void)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import IMP.Parser
import IMP.REPL
import IMP.Semantics

import qualified Paths_impli as Paths

data Action
    = Command String
    | File FilePath
    | STDIN
    | REPL
    | AST String
    | Version

actionParser :: Parser Action
actionParser =
    asum
        [ Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Print AST")
        , File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , flag' Version (long "version" <> short 'v' <> help "Print version")
        , pure REPL
        ]

actionInfo :: ParserInfo Action
actionInfo = info parser description
    where
        parser = actionParser <**> helper
        description =
            fullDesc
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> header "impli - The IMP language interpreter"

runCommand :: String -> IO ()
runCommand input = case parseProgram "command" input of
    Left err -> do
        putStrLn $ "ERROR: parse failure in: " ++ input
        putStrLn $ show err
        exitFailure
    Right stm -> void $ execStm initial stm

runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = void $ readIMP initial path

runREPL :: IO ()
runREPL = putStrLn "Welcome to the IMP REPL! Type :quit to exit" >> repl initial

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    case parseProgram "stdin" input of
        Left err -> do
            putStrLn $ "ERROR: parse failure"
            putStrLn $ show err
            exitFailure
        Right parsed -> void $ execStm initial parsed

version :: String
version = "impli " ++ showVersion Paths.version
