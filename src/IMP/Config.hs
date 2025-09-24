{- |
Module      : Config
Description : Configuration for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Configuration settings for the IMP language interpreter, REPL and CLI.
-}
module IMP.Config where

-- | Toggle to control IMP language semantics in 'IMP.Statement.interpret'.
operational :: Bool
operational = False -- INFO: structural semantics need less memory

-- | Toggle to control IMP language extension in 'IMP.Parser.parser'.
extensions :: Bool
extensions = True
