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
import System.Exit (exitFailure)

import IMP.Config
import IMP.Parser
import IMP.REPL
import IMP.Result
import IMP.Semantics.State
import IMP.Semantics.Statement

import qualified Paths_impli as Paths

{- FOURMOLU_DISABLE -}
-- | Mode to run the CLI.
data Mode
    = REPL              -- ^ Start the interactive REPL.
    | File FilePath     -- ^ Interpret IMP source file.
    | Command String    -- ^ Interpret IMP command passed as a string.
    | AST String        -- ^ Print the AST of a construct.
    | STDIN             -- ^ Interpret from standard input.
    | Version           -- ^ Print the version.
{- FOURMOLU_ENABLE -}

-- | Parser for the CLI mode.
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

-- | CLI parser information for optparse-applicative.
cli :: ParserInfo Mode
cli = info modi description
    where
        modi = modus <**> helper
        description =
            fullDesc
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> header "impli - The IMP language interpreter"

-- | Run the interactive REPL.
runREPL :: IO ()
runREPL = repl start

-- | Interpret a source file or standard input if path is @-@.
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

-- | Interpret a command passed as a string.
runCommand :: String -> IO ()
runCommand input = runProgram "command" input

-- | Interpret from standard input.
runSTDIN :: IO ()
runSTDIN = do
    input <- getContents
    runProgram "stdin" input

-- | Print AST of a construct.
runAST :: String -> IO ()
runAST input = runInputT settings $ do
    result <- -- handleInterrupt (return $ Left $ Sigint) $
        runExceptT $ printAST input
    liftIO $ case result of
        Left err -> do
            print err
            exitFailure
        Right _ -> return ()

-- | Print the program version.
runVersion :: IO ()
runVersion = putStrLn $ "impli " ++ showVersion Paths.version

-- | Run program with parse channel and input string.
runProgram :: String -> String -> IO ()
runProgram channel input = runInputT settings $
    case parser channel input of
        Left err -> liftIO $ do
            print $ ParseFail input
            print err
            exitFailure
        Right stm -> do
            result <- -- handleInterrupt (return $ Left $ Sigint) $
                runExceptT $ execute initial stm
            liftIO $ case result of
                Left err -> do
                    print err
                    exitFailure
                Right _ -> return ()
