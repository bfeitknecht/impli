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
module CLI (
    parseCLI,
    runCLI,
) where

import Config
import Control.Monad.Except (catchError)
import Data.Version (showVersion)
import IMP.Exception
import IMP.Parser
import IMP.REPL
import IMP.State
import IMP.Statement
import Options.Applicative
import System.Exit (exitFailure)

import qualified Control.Monad.Trans.Except as Except
import qualified Paths_impli as Paths
import qualified System.Console.Haskeline as Haskeline

-- | Mode to run the CLI.
{- FOURMOLU_DISABLE -}
data Mode
    = REPL Bool         -- ^ Start interactive REPL with option to toggle history.
    | File FilePath     -- ^ Interpret IMP source file.
    | Command String    -- ^ Interpret IMP command passed as string.
    | AST String        -- ^ Print the AST of a construct.
    | STDIN             -- ^ Interpret from standard input.
    | Version           -- ^ Print the version.
{- FOURMOLU_ENABLE -}

-- | Parser for the CLI mode.
parseMode :: Parser Mode
parseMode =
    asum
        [ REPL <$> switch (long "no-history" <> help "Disable REPL history")
        , File <$> strArgument (metavar "FILE" <> help "Interpret source file")
        , Command <$> strOption (long "command" <> short 'c' <> metavar "COMMAND" <> help "Interpret command")
        , AST <$> strOption (long "ast" <> short 'a' <> metavar "CONSTRUCT" <> help "Print AST")
        , flag' STDIN (long "stdin" <> help "Interpret from standard input")
        , flag' Version (long "version" <> short 'v' <> help "Print version")
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
                <> header "impli - IMP language interpreter"
                <> progDesc "An interpreter and REPL for the imperative toy language IMP"
                <> footer "For more information visit https://github.com/bfeitknecht/impli"

-- | Parse CLI options and return 'Mode' for execution.
parseCLI :: IO Mode
parseCLI = customExecParser defaultPrefs {prefColumns = 120} cli

-- | Entrypoint for the CLI.
runCLI :: Mode -> IO ()
runCLI mode =
    case mode of
        REPL h -> repl (settings h) start
        File path -> runFile path
        Command cmd -> runProgram "command" cmd
        AST input -> printAST input
        STDIN -> runSTDIN
        Version -> putStrLn $ "impli " ++ showVersion Paths.version

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
            Except.runExceptT (interpret (stm, initial))
                >>= either (\e -> print e >> exitFailure) (\_ -> return ())

-- | Default 'System.Console.Haskeline.Settings' for 'IMP.REPL.repl'.
settings :: Bool -> Haskeline.Settings IO
settings noHistory =
    Haskeline.defaultSettings
        { Haskeline.historyFile = if noHistory then Nothing else historyFile
        , Haskeline.autoAddHistory = True
        }
