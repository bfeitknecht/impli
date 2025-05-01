{-# LANGUAGE CPP #-}

module Main where

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

import IMP.CLI
import IMP.REPL
import IMP.TTY (isTTY)

main :: IO ()
main = do
    tty <- isTTY
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
            if tty
                then runREPL
                else runSTDIN
