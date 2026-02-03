{- |
Module      : Main
Description : Web entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web/WASM entrypoint for the IMP language interpreter.
This module implements the REPL loop without haskeline dependency.
Uses the polymorphic REPL monad from REPL.State with IO as the base monad.
-}
module Main where

import REPL.Execute.Browser
import REPL.State

-- | Entrypoint for web/WASM IMP interpreter
main :: IO ()
main = repl start >> main -- escape is impossible
