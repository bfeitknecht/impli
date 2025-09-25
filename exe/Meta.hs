{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TODO
-}
module Meta where

import Preset
import Text.Parsec

import IMP.Lexer
import IMP.Parser
import IMP.Syntax

-- | TODO
data Command
    = Help
    | Quit
    | Clear
    | Reset Aspect
    | Show Aspect
    | Load FilePath
    | Write FilePath
    | AST Element
    | Set Option
    deriving (Eq, Show)

-- | TODO
data Aspect
    = Vars
    | Procs
    | Flag
    | Trace
    | All
    deriving (Eq, Show)

-- | TODO
data Element
    = Index Int
    | Input Construct
    deriving (Eq, Show)

-- | Modifiable option in 'IMP.REPL.repl' through @:set@ and @:unset@.
data Option
    = Welcome String
    | Prompt String
    | Separator Char
    | Goodbye String
    | Verbose Level
    deriving (Eq, Show)

-- | TODO
data Defaults = Default
    { __welcome :: String
    , __prompt :: String
    , __separator :: Char
    , __goodbye :: String
    , __verbose :: Level
    }

-- | TODO
defaults :: Defaults
defaults =
    Default
        { __welcome = welcome
        , __prompt = prompt
        , __separator = separator
        , __goodbye = goodbye
        , __verbose = verbosity
        }

-- | TODO
instance Parses Aspect where
    parses =
        choice
            [ Vars <$ string "vars"
            , Procs <$ string "procs"
            , Flag <$ (string "break" <|> string "flag")
            , Trace <$ string "trace"
            , All <$ string ""
            ]

-- | TODO
instance Parses Element where
    parses =
        choice
            [ try (Index . fromInteger <$ char '#' <*> integer)
            , Input <$> parses
            ]

-- | TODO
instance Parses Option where
    parses =
        choice
            [ Welcome <$ string "welcome" <*> sentence
            , Prompt <$ string "prompt" <*> word
            , Separator <$ string "separator" <*> satisfy (`elem` ":?!-+*#%&$=>")
            , Goodbye <$ string "goodbye" <*> sentence
            , Verbose <$ string "verbose" <*> parses
            ]

-- | TODO
instance Parses Level where
    parses =
        choice
            [ Normal <$ string "normal"
            , Profile <$ string "profile"
            , Debug <$ string "debug"
            ]

-- | TODO
instance Parses Command where
    parses =
        choice
            [ Help <$ (command "help" <|> command "?")
            , Quit <$ command "quit"
            , Clear <$ command "clear"
            , Reset <$ command "reset" <*> parses
            , Show <$ command "show" <*> parses
            , Load <$ command "load" <*> filepath
            , Write <$ command "write" <*> filepath
            , AST <$ command "ast" <*> parses
            , Set <$> (set <|> unset)
            ]
        where
            set = command "set" *> parses
            unset =
                command "unset"
                    *> choice
                        [ Welcome <$ string "welcome" <*> return welcome
                        , Prompt <$ string "prompt" <*> return prompt
                        , Separator <$ string "separator" <*> return separator
                        , Goodbye <$ string "prompt" <*> return goodbye
                        , Verbose <$ string "prompt" <*> return verbosity
                        ]
