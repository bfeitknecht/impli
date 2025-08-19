{- |
Module      : Config
Description : Configuration for the IMP language interpreter and REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Configuration settings for IMP language interpreter and REPL environment.
-}
module Config where

-- | Toggle to control IMP language semantic.
operational :: Bool
operational = True

-- | Toggle to control IMP language extension.
extensions :: Bool
extensions = True

-- | Default message displayed on entry of the REPL.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | Default prompt displayed during execution of the REPL.
prompt :: String
prompt = "IMP> "

-- | Default message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | Default 'FilePath' to save history to.
historyFile :: Maybe FilePath
historyFile = Just ".imp_history"
