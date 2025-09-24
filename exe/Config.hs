module Config where

-- | Default message displayed on entry of 'IMP.REPL.repl'.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands and :quit to exit."

-- | Default prompt displayed during execution of the REPL.
prompt :: String
prompt = "IMP"

-- | Default message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | TODO
data Level = Normal | Profile | Debug

-- | TODO
verbosity :: Level
verbosity = Normal
