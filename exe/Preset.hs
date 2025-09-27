{- |
Module      : Preset
Description : Preset settings for the REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Defines preset options and constants for the REPL.
-}
module Preset where

-- | Default message displayed on entry of 'IMP.REPL.repl'.
welcome :: String
welcome = unwords ["Welcome to the IMP REPL!", hint]

-- | Default hint displayed on enter of 'IMP.REPL.repl' or metacommand error.
hint :: String
hint = "Enter :help to list available metacommands or :quit to exit."

-- | Default prompt displayed during execution of the REPL.
prompt :: String
prompt = "IMP"

-- | Default prompt-input separator.
normalsep :: Char
normalsep = '>'

-- | Default prompt-input separator with profile verbosity.
profilesep :: Char
profilesep = '+'

-- | Default prompt-input separator with debug verbosity.
debugsep :: Char
debugsep = '*'

-- | Default message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | Encapsulation of verbosity level in 'IMP.REPL.REPL'.
data Level
    = Normal
    | Profile
    | Debug
    deriving (Eq, Show)

-- | Default verbosity.
verbosity :: Level
verbosity = Normal

-- | Encapsulation of default options.
data Defaults = Default
    { __welcome :: String
    , __prompt :: String
    , __separator :: Char
    , __goodbye :: String
    , __verbose :: Level
    }

-- | Default options.
defaults :: Defaults
defaults =
    Default
        { __welcome = welcome
        , __prompt = prompt
        , __separator = normalsep
        , __goodbye = goodbye
        , __verbose = verbosity
        }

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
