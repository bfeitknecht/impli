{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : REPL.Meta
Description : Metastring definitions and parse instances
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the definitions of metacommands and associates.
Handles parsing of arguments and options.
-}
module REPL.Meta where

import Text.Parsec

import IMP.Lexer
import IMP.Parser
import IMP.Syntax
import REPL.Preset

-- | Encapsulation for metacommand.
data Command
    = Help
    | Quit
    | Clear
    | Version
    | Reset Aspect
    | Show Aspect
    | Load FilePath
    | Write FilePath
    | AST Element
    | Set Option
    deriving (Eq, Show)

-- | Encapsulation of an aspect to 'REPL.reset' or 'REPL.shower'.
data Aspect
    = Vars
    | Procs
    | Flag
    | Trace
    | All
    deriving (Eq, Show)

-- | Encapsulation of an element to 'REPL.ast'.
data Element
    = Index Int
    | Input Construct
    deriving (Eq, Show)

-- | Modifiable option in 'REPL.repl' through @:set@ and @:unset@.
data Option
    = Welcome String
    | Prompt String
    | Separator Char
    | Goodbye String
    | Verbose Level
    deriving (Eq, Show)

-- | Parser for showable and resettable aspect 'Aspect'.
instance Parses Aspect where
    parses =
        choice
            [ Vars <$ symbol "vars"
            , Procs <$ symbol "procs"
            , Flag <$ symbol "break"
            , Trace <$ symbol "trace"
            , All <$ return () -- INFO: empty word corresponds to every aspect
            ]

-- | Parser for element 'Element' to 'REPL.shower'.
instance Parses Element where
    parses =
        choice
            [ Index . fromInteger <$ char '#' <*> integer
            , Input <$> parses
            ]

-- | Parser for modifiable option 'Option'.
instance Parses Option where
    parses =
        choice
            [ Welcome <$ symbol "welcome" <*> sentence
            , Prompt <$ symbol "prompt" <*> (word <|> mempty)
            , Separator <$ symbol "separator" <*> satisfy (`elem` ":?!-+*#%&$=>")
            , Goodbye <$ symbol "goodbye" <*> sentence
            , Verbose <$ symbol "verbose" <*> parses
            ]

-- | Parser for verbosity level 'Level'.
instance Parses Level where
    parses =
        choice
            [ Normal <$ symbol "normal"
            , Profile <$ symbol "profile"
            , Debug <$ symbol "debug"
            ]

-- | Parser for metacommand 'Command'.
instance Parses Command where
    parses =
        choice
            [ Help <$ (command "help" <|> command "?")
            , Quit <$ command "quit"
            , Clear <$ command "clear"
            , Version <$ command "version"
            , Reset <$ command "reset" <*> parses
            , try $ Show <$ command "show" <*> parses
            , Load <$ command "load" <*> filepath
            , Write <$ command "write" <*> filepath
            , AST <$ command "ast" <*> parses
            , Set <$> (set <|> unset)
            ]
        where
            command cmd = try (symbol cmd) <|> symbol [head cmd] -- INFO: case of empty string not possible
            set = symbol "set" *> parses
            unset =
                symbol "unset"
                    *> choice
                        [ Welcome <$ symbol "welcome" <*> return welcome
                        , Prompt <$ symbol "prompt" <*> return prompt
                        , Separator <$ symbol "separator" <*> return normalsep
                        , Goodbye <$ symbol "prompt" <*> return goodbye
                        , Verbose <$ symbol "verbose" <*> return verbosity
                        ]
