{-# LANGUAGE TypeApplications #-}

{- |
Module      : CLI
Description : Commandline interface for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the commandline interface for the IMP language interpreter.
Handles parsing of arguments and options, then runs in the appropriate
execution mode, such as the REPL, file (or stdin) interpretation,
command execution or prints the version, help or AST of a construct.
-}
module CLI where

import Control.Monad.Except (catchError)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import qualified Control.Monad.Trans.Except as Except

import IMP.Exception
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL

import qualified Paths_impli as Paths

-- | Mode to run the CLI.
{- FOURMOLU_DISABLE -}
data Mode
    = REPL (Maybe FilePath) (Maybe FilePath)    -- ^ Start interactive REPL with customization
    | File FilePath                             -- ^ Interpret IMP source file.
    | Command String                            -- ^ Interpret IMP command passed as string.
    | AST String                                -- ^ Print the AST of a construct.
    | STDIN                                     -- ^ Interpret from standard input.
    | Version                                   -- ^ Print the version.
{- FOURMOLU_ENABLE -}

-- | Parser for the CLI mode.
parseMode :: Parser Mode
parseMode =
    asum
        [ REPL
            <$> option
                (Just <$> str)
                (long "history" <> metavar "FILE" <> help "Save REPL history" <> value Nothing)
            <*> option
                (Just <$> str)
                (long "config" <> metavar "FILE" <> help "Read Haskeline configuration" <> value Nothing)
        , File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Show abstract syntax tree")
        , flag' Version (long "version" <> help "Show version")
        ]

-- | Parser for the CLI options and information.
cli :: ParserInfo Mode
cli = info modifier description
    where
        modifier =
            parseMode
                <**> helper
        description =
            fullDesc
                <> header "impli - The IMP Language Interpreter"
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> footer "For more information visit https://github.com/bfeitknecht/impli"

-- | Parse CLI options and return 'Mode' for execution.
parseCLI :: IO Mode
parseCLI = customExecParser defaultPrefs {prefColumns = maxBound} cli

-- | Entrypoint for the CLI.
runCLI :: Mode -> IO ()
runCLI mode =
    case mode of
        REPL hist conf -> setup hist conf >>= flip repl start
        File path -> runFile path
        Command cmd -> runProgram "command" cmd
        AST input -> printAST input
        STDIN -> runSTDIN
        Version -> putStrLn $ unwords ["impli", showVersion Paths.version]

-- | Interpret source file or standard input if path is @-@.
runFile :: FilePath -> IO ()
runFile "-" = runSTDIN
runFile path = do
    content <-
        readFile path `catchError` \e -> do
            print . IOFail $ unlines ["read from: " ++ path, show e]
            exitFailure
    runProgram path content

-- | Interpret from standard input.
runSTDIN :: IO ()
runSTDIN = getContents >>= runProgram "stdin"

-- | Run program with parse channel and input string.
runProgram :: String -> String -> IO ()
runProgram channel input =
    case parser channel input of
        Left e -> do
            print . ParseFail $ unlines [input, show e]
            exitFailure
        Right stm ->
            Except.runExceptT (execute (stm, initial))
                >>= either (\e -> print e >> exitFailure) (\_ -> return ())

-- | Parse input and print AST.
printAST :: String -> IO ()
printAST input =
    either
        (\e -> print . ParseFail $ unlines [input, show e])
        print
        (parser @Construct "AST" input)
