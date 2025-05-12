module CLI where

import Control.Exception (catch)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import IMP.Parser.Parses
import IMP.REPL
import IMP.Semantics.State
import IMP.Semantics.Statement

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
runCommand input = handleUncaught $ case parseStm "command" input of
    Left err -> do
        putStrLn $ "*** ERROR: parse failure in: " ++ input
        putStrLn $ show err
        exitFailure
    Right stm -> execStm initial stm

runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = handleUncaught $ readIMP initial path

runREPL :: IO ()
runREPL = handleUncaught (putStrLn "Welcome to the IMP REPL! Type :quit to exit" >> repl initial)

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    handleUncaught $ case parseStm "stdin" input of
        Left err -> do
            putStrLn $ "*** ERROR: parse failure"
            putStrLn $ show err
            exitFailure
        Right parsed -> execStm initial parsed

version :: String
version = "impli " ++ showVersion Paths.version

handleUncaught :: IO a -> IO ()
handleUncaught execution = do
    _ <- catch execution handle
    return ()
    where
        handle (Throw v) = do
            putStrLn $ "*** ERROR: uncaught exception with value: " ++ show v
            exitFailure
