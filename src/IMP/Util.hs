{- |
Module      : IMP.Util
Description : Utility functions for the IMP language parser
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides utility functions and definitions used in the
IMP parser. It includes token parsers, reserved keywords, operators,
and helper functions for parsing identifiers, symbols, and whitespace.
-}
module IMP.Util where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

-- | Reserved keywords.
keywords :: [String]
keywords =
    [ "skip"
    , "if"
    , "then"
    , "else"
    , "end"
    , "while"
    , "do"
    , "true"
    , "false"
    , "par"
    , "[]"
    , "print"
    , "read"
    , "var"
    , "in"
    , "procedure"
    , "begin"
    , "break"
    , "for"
    , "to"
    , "repeat"
    , "until"
    , "time"
    , "match"
    , "on"
    , "default"
    , "havoc"
    , "assert"
    , "flip"
    , "flop"
    , "raise"
    , "try"
    , "catch"
    , "with"
    , "swap"
    ]

-- | Reserved operators.
operators :: [String]
operators =
    [ "+"
    , "-"
    , "*"
    , "/"
    , "%"
    , "="
    , "#"
    , "<"
    , "<="
    , ">"
    , ">="
    , "not"
    , "and"
    , "or"
    , ":="
    , "+="
    , "-="
    , "*="
    , "/="
    , "%="
    ]

-- | Reserved metta-commands.
metacommands :: [String]
metacommands =
    [ ":help"
    , ":quit"
    , ":clear"
    , ":reset"
    , ":trace"
    , ":state"
    , ":load"
    , ":write"
    , ":ast"
    ]

-- | Token parser for IMP, including operators and reserved keywords.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops = operators
        keys = keywords
        style =
            emptyDef
                { Tok.commentLine = "//"
                , Tok.commentStart = "/*"
                , Tok.commentEnd = "*/"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = keys
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum
                }

-- | Parses an identifier (variable or procedure name).
identifier :: Parser String
identifier = (Tok.identifier lexer) <?> "identifier"

-- | Parses a variable name or the placeholder '_'.
variable :: Parser String
variable = (identifier <|> symbol "_") <?> "variable"

-- | Parses a keyword.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Parses a reserved keyword and adds label to it.
keyword :: String -> Parser ()
keyword x = reserved x <?> x

-- | Parses an operator.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Parses a reserved operator and adds label to it.
operator :: String -> Parser ()
operator x = reservedOp x <?> x

-- | Parses parentheses around another parser.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parses an integer literal.
integer :: Parser Integer
integer = (Tok.integer lexer) <?> "integer"

-- | Parses whitespace.
whitespace :: Parser ()
whitespace = (Tok.whiteSpace lexer) <?> "whitespace"

-- | Parses a symbol with label.
symbol :: String -> Parser String
symbol x = (Tok.symbol lexer x) <?> x
