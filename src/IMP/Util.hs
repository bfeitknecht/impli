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

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops =
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
            , "par"
            , "[]"
            ]
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
        style =
            emptyDef
                { Tok.commentLine = "//"
                , Tok.commentStart = "/*"
                , Tok.commentEnd = "*/"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = keywords
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum
                }

identifier :: Parser String
identifier = Tok.identifier lexer

variable :: Parser String
variable = identifier <|> symbol "_"

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

operator :: String -> Parser ()
operator = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Integer
integer = Tok.integer lexer

semi :: Parser String
semi = Tok.semi lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer
