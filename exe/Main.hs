module Main where

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, stdin)

import CLI
import IMP.REPL

main :: IO ()
main = do
    tty <- hIsTerminalDevice stdin
    invoc <- execParser actionInfo
    case invoc of
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
            if tty
                then runREPL
                else runSTDIN
