{- |
Module      : REPL.Util
Description : Haskeline-based REPL utilities
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides haskeline-based REPL type and re-exports parametric functions from REPL.State.
-}
module REPL.Util (
    -- Re-export from REPL.State
    module REPL.State,
    -- Haskeline-specific types
    REPL,
    -- Haskeline-specific functions
    clear,
) where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.Console.Haskeline hiding (display)

import qualified System.Console.ANSI as ANSI

import IMP.Exception
import REPL.State

-- | Encapsulation of computation in 'REPL.repl' with haskeline.
type REPL = StateT Store (ExceptT Exception (InputT IO))

-- | Clear the terminal (haskeline-specific).
clear :: REPL ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)
