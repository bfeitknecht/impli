module Config where

import System.Console.Haskeline

-- | __TODO__
operational :: Bool
operational = True

-- | __TODO__
extensions :: Bool
extensions = True

-- | __TODO__
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | __TODO__
prompt :: String
prompt = "IMP> "

-- | __TODO__
goodbye :: String
goodbye = "Goodbye!"

-- | __TODO__
settings :: Settings IO
settings =
    defaultSettings
        { historyFile = Just ".imp_history"
        , autoAddHistory = True
        }
