{- |
Module      : Main
Description : Entrypoint of the @impli@ executable
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module serves as the main entry point for the @impli@ executable. It parses
command-line arguments to determine the mode of operation and then delegates
execution to the appropriate API.
-}
module Main where

import Options.Applicative

import CLI

-- | The main entry point for the @impli@ executable.
main :: IO ()
main = execParser cli >>= runCLI
