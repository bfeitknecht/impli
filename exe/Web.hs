{- |
Module      : Main
Description : Web capability for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web capabilities for the IMP language interpreter.
-}
module Main where

import Control.Monad.Except
import Control.Monad.State
import System.Console.Haskeline
import System.Exit (exitFailure)

import Preset
import REPL hiding (repl)
import Util

-- | Entrypoint for the IMP language interpreter in the web.
main :: IO ()
main = repl start

-- | Read-Evaluate-Print-Loop in the 'REPL' monad.
repl :: Store -> IO ()
repl store = do
    putStrLn $ _welcome store
    runInputTWithPrefs
        defaultPrefs
        defaultSettings
        (withInterrupt (runExceptT (execStateT loop store)))
        >>= either (\e -> print e >> exitFailure) (\store' -> putStrLn goodbye >> repl store')
    error "How did we get here?"
