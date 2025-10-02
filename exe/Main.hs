{- |
Module      : Main
Description : Entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Defines the main function which serves as entrypoint for the IMP language interpreter.
-}
module Main where

import CLI

-- | Entrypoint for the IMP language interpreter.
main :: IO ()
main = parseCLI >>= runCLI
