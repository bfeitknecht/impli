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
module Config where

-- | Toggle to control IMP language semantics in 'IMP.Statement.interpret'.
operational :: Bool
operational = False -- INFO: structural semantics need less memory

-- | Toggle to control IMP language extension in 'IMP.Parser.parser'.
extensions :: Bool
extensions = True

-- | Default message displayed on entry of 'IMP.REPL.repl'.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

wwwelcome :: String
wwwelcome = "Welcome to the IMP REPL! Enter :help to list available metacommands."

-- | Default prompt displayed during execution of the REPL.
prompt :: String
prompt = "IMP> "

-- | Default message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | Default 'FilePath' to save history to.
historyFile :: Maybe FilePath
historyFile = Just ".imp_history"

-- | TODO: rename the above to the below.
history :: Maybe FilePath
history = historyFile
