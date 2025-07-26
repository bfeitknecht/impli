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
of a given construct. The CLI also includes options for reading input from
standard input and displaying the version.
-}
module CLI (
    parseCLI,
    runCLI,
) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Version (showVersion)
import Options.Applicative
import System.Console.Haskeline (runInputT)
import System.Exit (exitFailure)

import IMP.Config
import IMP.Parser
import IMP.REPL
import IMP.Result
import IMP.Semantics.State
import IMP.Semantics.Statement
import IMP.Util

import qualified Paths_impli as Paths

{- FOURMOLU_DISABLE -}
-- | Mode to run the CLI.
data Mode
    = REPL              -- ^ Start the interactive REPL.
    | File FilePath     -- ^ Interpret IMP source file.
    | Command String    -- ^ Interpret IMP command passed as a string.
    | AST String        -- ^ Show the AST of a construct.
    | STDIN             -- ^ Interpret from standard input.
    | Version           -- ^ Print the version.
{- FOURMOLU_ENABLE -}

-- | Parser for the CLI mode.
parseMode :: Parser Mode
parseMode =
    asum
        [ pure REPL
        , File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Show AST")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , flag' Version (long "version" <> short 'v' <> help "Show version")
        ]

-- | CLI parser information for optparse-applicative.
cli :: ParserInfo Mode
cli = info modifier description
    where
        modifier =
            parseMode
                <**> helper
        description =
            fullDesc
                <> header "impli - IMP language interpreter"
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> footer "For more information visit https://github.com/bfeitknecht/impli"

parseCLI :: IO Mode
parseCLI = customExecParser defaultPrefs {prefColumns = 120} cli

-- | Entry point for the CLI application
runCLI :: Mode -> IO ()
runCLI mode =
    case mode of
        REPL -> runREPL
        File path -> runFile path
        Command cmd -> runCommand cmd
        AST construct -> runAST construct
        STDIN -> runSTDIN
        Version -> runVersion

-- | Run the interactive REPL.
runREPL :: IO ()
runREPL = repl settings start

-- | Interpret a source file or standard input if path is @-@.
runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left err -> print (IOFail $ unlines' ["read from: " ++ path, show err]) >> exitFailure
        Right content -> runProgram path content

-- | Interpret a command passed as a string.
runCommand :: String -> IO ()
runCommand = runProgram "command"

-- | Print AST of a construct.
runAST :: String -> IO ()
runAST input = runInputT nohistory $ do
    result <- runExceptT $ printAST input
    liftIO $ case result of
        Left err -> print err >> exitFailure
        Right _ -> return ()

-- | Interpret from standard input.
runSTDIN :: IO ()
runSTDIN = getContents >>= runProgram "stdin"

-- | Print the program version.
runVersion :: IO ()
runVersion = putStrLn $ "impli " ++ showVersion Paths.version

-- | Run program with parse channel and input string.
runProgram :: String -> String -> IO ()
runProgram channel input = runInputT nohistory $
    case parser channel input of
        Left err -> liftIO $ print (ParseFail $ unlines' [input, show err]) >> exitFailure
        Right stm -> do
            result <- runExceptT $ interpret initial stm
            liftIO $ case result of
                Left err -> print err >> exitFailure
                Right _ -> return ()
