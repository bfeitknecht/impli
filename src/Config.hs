module Config where

import System.Console.Haskeline

operational :: Bool
operational = True

welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

prompt :: String
prompt = "IMP> "

goodbye :: String
goodbye = "Goodbye!"

settings :: Settings IO
settings =
    defaultSettings
        { historyFile = Just ".imp_history"
        , autoAddHistory = True
        }
