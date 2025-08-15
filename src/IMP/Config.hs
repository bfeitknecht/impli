{- |
Module      : IMP.Config
Description : REPL configurations and settings
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines Haskeline settings for the IMP REPL.
It provides options used by "IMP.REPL" to configure
the interactive interpreter, including prompt text,
welcome and goodbye messages, and history settings.
-}
module IMP.Config (
    welcome,
    prompt,
    goodbye,
    small,
    settings,
    nohistory,
) where

import System.Console.Haskeline

-- | Welcome message displayed when 'IMP.Semantic.Statement.repl' is started.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | Prompt in 'IMP.Semantic.Statement.repl' displayed before each input line.
prompt :: String
prompt = "IMP> "

-- | Goodbye message displayed when 'IMP.Semantic.Statement.repl' is quit with @:quit@.
goodbye :: String
goodbye = "Goodbye!"

-- | REPL settings for "System.Console.Haskeline".
-- Used to configure the execution mode in "IMP.CLI".
settings :: Settings IO
settings =
    defaultSettings
        { historyFile = Just ".imp_history"
        , autoAddHistory = True
        }

-- | REPL settings with no historyFile.
-- Used in "IMP.CLI" when history persistence is not desired.
nohistory :: Settings IO
nohistory = settings {historyFile = Nothing}

-- | Use small-step semantics for interpretation in 'IMP.Semantic.Statement.interpret'.
-- When @True@, "IMP.Semantic.Statement" uses small-step semantics, i.e. 'IMP.Semantic.Statement.steps'.
-- Otherwise it uses big-step semantics, i.e. 'IMP.Semantic.Statement.run'.
small :: Bool
small = True
