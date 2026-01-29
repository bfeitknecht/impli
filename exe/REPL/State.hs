{- |
Module      : REPL.State
Description : Shared state definitions for REPL modules
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides shared state and store definitions for REPL modules.
-}
module REPL.State where

import IMP.State (State, initial)
import IMP.Syntax (Stm)
import REPL.Meta
import REPL.Preset

-- | Encapsulation of state in REPL.
data Store = Store
    { _state :: State
    , _trace :: [Stm]
    , _defaults :: Defaults
    , _welcome :: String
    , _prompt :: String
    , _separator :: Char
    , _goodbye :: String
    , _verbose :: Level
    }

-- | Starting data store for REPL.
start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , _defaults = defaults
        , _welcome = welcome
        , _prompt = prompt
        , _separator = normalsep
        , _goodbye = goodbye
        , _verbose = verbosity
        }
