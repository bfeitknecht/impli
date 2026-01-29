{- |
Module      : Main
Description : Web entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web/WASM entrypoint for the IMP language interpreter.
-}
module Main where

import REPL.State (start)
import REPL.Web (repl)

-- | Entrypoint for web/WASM IMP interpreter
main :: IO ()
main = repl start
