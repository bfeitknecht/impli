{- |
Module      : CLI
Description : Command-line interface for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides the command-line interface for the IMP language interpreter.
It supports various modes of operation, including running the REPL, interpreting
source files, executing commands directly, and printing the abstract syntax tree (AST)
of a given construct. The CLI also includes options for reading input from standard
input and displaying the version.
-}
module CLI where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Version (showVersion)
import Options.Applicative
import System.Console.Haskeline
import System.Exit (exitFailure, exitSuccess)

import IMP.Parser
import IMP.REPL
import IMP.Result
import IMP.Semantics.State
import IMP.Semantics.Statement

import qualified Paths_impli as Paths

data Mode
    = REPL
    | File FilePath
    | Command String
    | AST String
    | STDIN
    | Version

modus :: Parser Mode
modus =
    asum
        [ pure REPL
        , File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Print AST")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , flag' Version (long "version" <> short 'v' <> help "Print version")
        ]

cli :: ParserInfo Mode
cli = info modi description
    where
        modi = modus <**> helper
        description =
            fullDesc
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> header "impli - The IMP language interpreter"

runREPL :: IO ()
runREPL = putStrLn "Welcome to the IMP REPL! Type :quit to exit" >> repl start

runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left err -> do
            print $ IOFail $ "read from: " ++ path
            print err
            exitFailure
        Right content -> runProgram path content

runCommand :: String -> IO ()
runCommand input = runProgram "command" input

runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    runProgram "stdin" input

runAST :: String -> IO ()
runAST input = runInputT defaultSettings $ do
    result <- runExceptT $ printAST input
    liftIO $ case result of
        Left err -> do
            print err
            exitFailure
        Right _ -> exitSuccess

runVersion :: IO ()
runVersion = putStrLn $ "impli " ++ showVersion Paths.version

runProgram :: String -> String -> IO ()
runProgram channel input = runInputT defaultSettings $
    case parser channel input of
        Left err -> liftIO $ do
            print $ ParseFail input
            print err
            exitFailure
        Right stm -> do
            result <- runExceptT $ execute initial stm
            liftIO $ case result of
                Left err -> do
                    print err
                    exitFailure
                Right _ -> return ()
