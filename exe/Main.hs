{- |
Module      : Main
Description : Entrypoint for the @impli@ executable
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

__TODO__
-}
module Main where

import CLI

-- | Entrypoint for the @impli@ executable.
main :: IO ()
main = parseCLI >>= runCLI
