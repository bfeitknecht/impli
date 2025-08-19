{- |
Module      : IMP.Lexer
Description : TODO
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.Lexer where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Token as Token

-- | TODO
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
    , "timeout"
    , "after"
    , "alternate"
    ]

-- | TODO
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

-- | TODO
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where
        style =
            emptyDef
                { Token.commentLine = "//"
                , Token.commentStart = "/*"
                , Token.commentEnd = "*/"
                , Token.reservedOpNames = operators
                , Token.reservedNames = keywords
                , Token.identStart = letter
                , Token.identLetter = alphaNum
                }

-- | TODO
identifier :: Parser String
identifier = Token.identifier lexer <?> "identifier"

-- | TODO
variable :: Parser String
variable = identifier <|> symbol "_" <?> "variable"

-- | TODO
reserved :: String -> Parser ()
reserved = Token.reserved lexer

-- | TODO
keyword :: String -> Parser ()
keyword x = reserved x <?> x

-- | TODO
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- | TODO
operator :: String -> Parser ()
operator x = reservedOp x <?> x

-- | TODO
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | TODO
integer :: Parser Integer
integer = Token.integer lexer <?> "integer"

-- | TODO
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer <?> "whitespace"

-- | TODO
symbol :: String -> Parser String
symbol x = Token.symbol lexer x <?> x
