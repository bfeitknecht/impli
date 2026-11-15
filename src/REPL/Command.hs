{- |
Module      : REPL.Command
Description : Metacommands of the IMP language REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Metacommands of the REPL, their parser and the help message.
Metacommands start with a colon and can be abbreviated by their first letter.
-}
module REPL.Command (
    Command (..),
    Aspect (..),
    Element (..),
    Option (..),
    Level (..),
    commands,
    helpText,
) where

import Text.Parsec

import IMP.Interpret (Semantics (..))
import IMP.Lexer
import IMP.Parser
import IMP.Syntax

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

-- | Aspect of the session to show or reset.
data Aspect
    = All
    | Vars
    | Procs
    | Trace
    deriving (Eq, Show)

-- | Element to inspect via @:ast@.
data Element
    = Index Int
    | Input Construct
    deriving (Eq, Show)

-- | Setting changed via @:set@.
data Option
    = Interpretation Semantics
    | Verbose Level
    deriving (Eq, Show)

-- | Verbosity level, @profile@ reports resource usage and @debug@ additionally
-- the parsed statement and steps of structural semantics.
data Level
    = Normal
    | Profile
    | Debug
    deriving (Bounded, Enum, Eq, Show)

-- | Names of metacommands, for completion.
commands :: [String]
commands = [":help", ":quit", ":clear", ":version", ":reset", ":show", ":load", ":write", ":ast", ":set"]

-- | Help message displayed when user enters @:help@ metacommand.
helpText :: String
helpText =
    init . unlines $
        "All metacommands besides :set can be abbreviated by their first letter"
            : map ("    " ++) entries
    where
        entries =
            [ ":help                   Show this help message"
            , ":quit                   Quit REPL"
            , ":clear                  Clear screen"
            , ":version                Show version"
            , ":reset [ASPECT]         Reset session or aspect (vars, procs, trace)"
            , ":show [ASPECT]          Show session or aspect"
            , ":load FILE              Interpret file and load resulting state"
            , ":write FILE             Write trace to file"
            , ":ast (INPUT | #n)       Show AST of input or n-th statement in trace"
            , ":set semantics SEM      Interpret with semantics (natural, structural)"
            , ":set verbose LEVEL      Set verbosity level (normal, profile, debug)"
            ]

-- | Parser for aspect.
instance Parses Aspect where
    parses =
        choice
            [ Vars <$ symbol "vars"
            , Procs <$ symbol "procs"
            , Trace <$ symbol "trace"
            , All <$ return () -- INFO: Empty word means all aspects
            ]

-- | Parser for element to inspect.
instance Parses Element where
    parses =
        choice
            [ Index . fromInteger <$ char '#' <*> integer
            , Input <$> parses
            ]

-- | Parser for setting.
instance Parses Option where
    parses =
        choice
            [ Interpretation
                <$ symbol "semantics"
                <*> choice [Natural <$ symbol "natural", Structural <$ symbol "structural"]
            , Verbose
                <$ symbol "verbose"
                <*> choice [Normal <$ symbol "normal", Profile <$ symbol "profile", Debug <$ symbol "debug"]
            ]

-- | Parser for metacommand, without leading colon.
instance Parses Command where
    parses =
        choice
            [ Help <$ (command "help" <|> command "?")
            , Quit <$ command "quit"
            , Clear <$ command "clear"
            , Version <$ command "version"
            , Set <$ try (symbol "set") <*> parses
            , Reset <$ command "reset" <*> parses
            , Show <$ command "show" <*> parses
            , Load <$ command "load" <*> filepath
            , Write <$ command "write" <*> filepath
            , AST <$ command "ast" <*> parses
            ]
        where
            command name@(c : _) = whole name <|> whole [c]
            command [] = parserZero
            -- INFO: Check the word boundary before whitespace, so the abbreviation in @:l FILE@ parses
            whole w = try (string w <* notFollowedBy alphaNum) <* whitespace
