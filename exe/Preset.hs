{- |
TODO
-}
module Preset where

-- | Default message displayed on entry of 'IMP.REPL.repl'.
welcome :: String
welcome = "Welcome to the IMP REPL!" ++ hint

-- | TODO
hint :: String
hint = "Enter :help to list available metacommands or :quit to exit."

-- | Default prompt displayed during execution of the REPL.
prompt :: String
prompt = "IMP"

-- | TODO
separator :: Char
separator = '>'

-- | Default message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | TODO
data Level
    = Normal
    | Profile
    | Debug
    deriving (Eq, Show)

-- | TODO
verbosity :: Level
verbosity = Normal

-- | Help message displayed when user enters @:help@ metacommand.
helpMessage :: [String]
helpMessage =
    [ ":help                    Show this help message"
    , ":quit                    Quit REPL"
    , ":clear                   Clear screen"
    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
    , ":show [ASPECT]           Show environment or specific aspect"
    , ":load FILE               Interpret file and load resulting state"
    , ":write FILE              Write trace to file (relative to $PWD)"
    , ":ast (INPUT | #n)        Parse and display AST of input or n-th statement in trace"
    ]
