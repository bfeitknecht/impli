module Config where

import System.Console.Haskeline

-- | TODO
operational :: Bool
operational = True

-- | TODO
extensions :: Bool
extensions = True

-- | TODO
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | TODO
prompt :: String
prompt = "IMP> "

-- | TODO
goodbye :: String
goodbye = "Goodbye!"

-- | TODO
settings :: Settings IO
settings =
    defaultSettings
        { historyFile = Just ".imp_history"
        , autoAddHistory = True
        }
