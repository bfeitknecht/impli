{- |
Module      : Frontend.Native.Meta
Description : Metastring definitions and parse instances
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the definitions of metacommands and associates.
Handles parsing of aspects, elements and verbosity levels.
-}
module Frontend.Native.Meta (
    Command (..),
    Element (..),
    Level (..),
    levelName,
    parseLevel,
    helpText,
) where

import Text.Parsec

import IMP.Engine (Aspect (..))
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
    | Verbose Level
    deriving (Eq, Show)

-- | Encapsulation of an element to inspect via @:ast@.
data Element
    = Index Int
    | Input Construct
    deriving (Eq, Show)

-- | Verbosity level of the REPL.
data Level
    = Normal
    | Profile
    | Debug
    deriving (Eq, Show)

-- | Name of verbosity level.
levelName :: Level -> String
levelName level = case level of
    Normal -> "normal"
    Profile -> "profile"
    Debug -> "debug"

-- | Parse verbosity level name, for CLI option parsing.
parseLevel :: String -> Either String Level
parseLevel input = case input of
    "normal" -> Right Normal
    "profile" -> Right Profile
    "debug" -> Right Debug
    _ -> Left $ "unknown verbosity level: " ++ input

-- | Help message displayed when user enters @:help@ metacommand.
helpText :: String
helpText =
    unlines $
        "All metacommands besides :set can be abbreviated by their first letter"
            : map ("    " ++) helpMessage

-- | Entries of the help message.
helpMessage :: [String]
helpMessage =
    [ ":help                    Show this help message"
    , ":quit                    Quit REPL"
    , ":clear                   Clear screen"
    , ":version                Show version"
    , ":set verbose LEVEL      Set verbosity level (normal, profile, debug)"
    , ":reset [ASPECT]         Reset environment or specific aspect (vars, procs, break, trace)"
    , ":show [ASPECT]          Show environment or specific aspect"
    , ":load FILE              Interpret file and load resulting state"
    , ":write FILE             Write trace to file (relative to $PWD)"
    , ":ast (INPUT | #n)       Parse and display AST of input or n-th statement in trace"
    ]

-- | Parser for showable and resettable aspect 'IMP.Engine.Aspect'.
instance Parses Aspect where
    parses =
        choice
            [ Vars <$ symbol "vars"
            , Procs <$ symbol "procs"
            , Flag <$ symbol "break"
            , Trace <$ symbol "trace"
            , All <$ return () -- INFO: Empty word means all aspects
            ]

-- | Parser for element 'Element' to inspect.
instance Parses Element where
    parses =
        choice
            [ Index . fromInteger <$ char '#' <*> integer
            , Input <$> parses
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
            , Verbose <$ (try (symbol "set") *> symbol "verbose") <*> parses
            , Reset <$ command "reset" <*> parses
            , Show <$ command "show" <*> parses
            , Load <$ command "load" <*> filepath
            , Write <$ command "write" <*> filepath
            , AST <$ command "ast" <*> parses
            ]
        where
            command cmd = try (symbol cmd) <|> symbol [head cmd] -- INFO: Case of empty string not possible
