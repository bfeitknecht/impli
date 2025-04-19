module Main where

import Options.Applicative
import System.Console.Haskeline

import IMP.CLI
import IMP.REPL

main :: IO ()
main = do
    action <- execParser actionInfo
    case action of
        RunREPL -> runREPL
        RunSTDIN -> runSTDIN
        RunFile path -> runFile path
        RunCommand command -> runCommand command
        PrintAST input -> runInputT defaultSettings (printAST input)
