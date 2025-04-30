module Main where

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import IMP.CLI
import IMP.REPL

main :: IO ()
main = do
    isTTY <- queryTerminal stdInput
    action <- execParser actionInfo
    case action of
        STDIN -> runSTDIN
        File path -> runFile path
        Command input -> runCommand input
        Version -> putStrLn version
        AST input -> do
            success <- printAST input
            if success
                then exitSuccess
                else exitFailure
        REPL ->
            if isTTY
                then runREPL
                else runSTDIN
