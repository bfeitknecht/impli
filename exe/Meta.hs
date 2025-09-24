{-# OPTIONS_GHC -Wno-orphans #-}

module Meta where

import Config
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
    | Show (Either Aspect Option)
    | Load FilePath
    | Write FilePath
    | AST Element
    | Set Option
    | Unset Option

-- | TODO
data Aspect
    = Vars
    | Procs
    | Flag
    | Trace
    | All

-- | TODO
data Element
    = Index Int
    | Input Construct

-- | Modifiable option in 'IMP.REPL.repl' through @:set@ and @:unset@.
data Option
    = Welcome String
    | Prompt String
    | Goodbye String
    | Verbose Level

-- | TODO
data Defaults = Defaults
    { _welcome :: String
    , _prompt :: String
    , _goodbye :: String
    , _verbose :: Level
    }

-- | TODO
instance Parses Aspect where
    parses =
        choice
            [ Vars <$ string "vars"
            , Procs <$ string "procs"
            , Flag <$ (string "break" <|> string "flag")
            , Trace <$ string "trace"
            , All <$ string "all"
            ]

-- | TODO
instance Parses Element where
    parses = Index . fromInteger <$> integer <|> Input <$> parses

-- | TODO
instance Parses Option where
    parses =
        choice
            [ Welcome <$ string "welcome" <*> sentence
            , Prompt <$ string "prompt" <*> word
            , Goodbye <$ string "goodbye" <*> sentence
            , Verbose <$ string "verbose" <*> parses
            ]

-- | TODO
instance Parses Level where
    parses =
        choice
            [ Normal <$ string "normal"
            , Profile <$ string "info"
            , Debug <$ string "debug"
            ]

-- | TODO
instance Parses Command where
    parses =
        choice
            [ Help <$ (string "help" <|> string "?")
            , Quit <$ string "quit"
            , Clear <$ string "clear"
            , Reset <$ string "reset" <*> parses
            , Show <$ string "show" <*> (Left <$> try parses <|> Right <$> parses)
            , Load <$ string "load" <*> filepath
            , Write <$ string "write" <*> filepath
            , AST <$ string "ast" <*> parses
            , Set <$> (set <|> unset)
            ]
        where
            set = string "set" *> parses
            unset =
                string "unset"
                    *> choice
                        [ Welcome <$ string "welcome" <*> string welcome
                        , Prompt <$ string "prompt" <*> string prompt
                        , Goodbye <$ string "prompt" <*> string goodbye
                        , Verbose <$ string "prompt" <*> return verbosity
                        ]
