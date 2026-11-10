{-# LANGUAGE TypeApplications #-}

{- |
Module      : Frontend.Native.CLI
Description : Commandline interface for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the commandline interface for the IMP language interpreter.
Handles parsing of arguments and options, then runs in the appropriate
mode: the interactive REPL, file (or stdin) interpretation, command
execution, or printing the version, help or AST of a construct.
-}

module Frontend.Native.CLI (
    parseCLI,
    runCLI,
) where

import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitFailure)

import qualified Control.Exception as Exception

import Frontend.Native.Meta (Level (..), levelName, parseLevel)
import Frontend.Native.REPL (Options (..), interpret, repl)
import IMP.Engine (Semantics (..))
import IMP.Exception
import IMP.Parser
import IMP.Syntax

import qualified Paths_impli as Paths

-- | Mode to run the CLI.
{- FOURMOLU_DISABLE -}
data Mode
    = REPL                                  -- ^ Start interactive REPL.
    | File FilePath                         -- ^ Interpret IMP source file.
    | Command String                        -- ^ Interpret IMP command passed as string.
    | AST String                            -- ^ Print the AST of a construct.
    | STDIN                                 -- ^ Interpret from standard input.
    | Version                               -- ^ Print the version.
{- FOURMOLU_ENABLE -}

-- | Parser for the CLI mode.
mode :: Parser Mode
mode =
    asum
        [ File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Show abstract syntax tree")
        , flag' Version (long "version" <> help "Show version")
        , pure REPL
        ]

-- | Parser for the CLI frontend options.
options :: Parser Options
options =
    Options
        <$> option
            (eitherReader parseLevel)
            ( long "verbose"
                <> short 'v'
                <> metavar "LEVEL"
                <> help "Verbosity level: normal, profile or debug"
                <> value Normal
                <> showDefaultWith levelName
            )
        <*> flag Natural Structural (long "structural" <> help "Interpret with structural (small-step) semantics")
        <*> option
            (Just <$> str)
            (long "history" <> metavar "FILE" <> help "Save REPL history" <> value Nothing)

-- | Parser for the CLI options and information.
cli :: ParserInfo (Mode, Options)
cli = info modifier description
    where
        modifier = (,) <$> mode <*> options <**> helper
        description =
            fullDesc
                <> header "impli - The IMP Language Interpreter"
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> footer "For more information visit https://github.com/bfeitknecht/impli"

-- | Parse CLI options into mode and frontend options.
parseCLI :: IO (Mode, Options)
parseCLI = customExecParser defaultPrefs {prefColumns = maxBound} cli

-- | Entrypoint for the CLI.
runCLI :: (Mode, Options) -> IO ()
runCLI (modus, options) =
    case modus of
        REPL -> repl options
        File path -> runFile path
        Command cmd -> interpret (optSemantics options) "command" cmd
        AST input -> printAST input
        STDIN -> getContents >>= interpret (optSemantics options) "stdin"
        Version -> putStrLn $ unwords ["impli", showVersion Paths.version]
    where
        runFile "-" = getContents >>= interpret (optSemantics options) "stdin"
        runFile path = do
            content <- Exception.try (readFile path)
            case content of
                Left e -> do
                    print . IOFail $ unlines ["read from: " ++ path, show (e :: Exception.IOException)]
                    exitFailure
                Right input -> interpret (optSemantics options) path input

-- | Parse input and print AST.
printAST :: String -> IO ()
printAST input =
    either
        (\e -> print (ParseFail (unlines [input, show e])) >> exitFailure)
        print
        (parser @Construct "AST" input)
