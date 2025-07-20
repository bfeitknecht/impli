{- |
Module      : IMP.Config
Description : REPL configurations and settings
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines Haskeline settings for the IMP REPL.
-}
module IMP.Config where

import System.Console.Haskeline

-- | The welcome message printed when the REPL is started.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | The REPL prompt.
prompt :: String
prompt = "IMP> "

-- | The file to save the REPL history to.
historyfile :: Maybe FilePath
historyfile = Just ".imp_history"

-- | The goodbye message displayed when the REPL is quit.
goodbye :: String
goodbye = "Goodbye!"

-- | Automatically save the REPL history .
autohistory :: Bool
autohistory = True

-- | REPL Settings for Haskeline.
settings :: Settings IO
settings =
    (defaultSettings :: Settings IO)
        { historyFile = historyfile
        , autoAddHistory = autohistory
        }

-- | Use small-step semantics for interpretation.
small :: Bool
small = True
