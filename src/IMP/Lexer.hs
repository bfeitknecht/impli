{- |
Module      : IMP.Lexer
Description : Lexer for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Lexical analyzer for the IMP language. Defines lexical elements like keywords,
operators, identifiers, and whitespace, and provides parsers for them.
-}
module IMP.Lexer where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Token as Token

-- | List of reserved keywords in the IMP language.
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

-- | List of operator symbols in the IMP language.
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

-- | TokenParser configuration for the parsers of the IMP language.
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

-- | Parser for identifier, i.e. definable variable and procedure name.
identifier :: Parser String
identifier = Token.identifier lexer <?> "identifier"

-- | Parser for variable names, i.e. identifier or underscore (placeholder discard variable).
variable :: Parser String
variable = identifier <|> symbol "_" <?> "variable"

-- | Parser for reserved keyword, expectation attached.
keyword :: String -> Parser ()
keyword x = Token.reserved lexer x <?> x

-- | Parser for reserved operator, expectation attached.
operator :: String -> Parser ()
operator x = Token.reservedOp lexer x <?> x

-- | Parser for parentheses.
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | Parser for integer literals.
integer :: Parser Integer
integer = Token.integer lexer <?> "integer"

-- | Parser for whitespace, including comments.
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer <?> "whitespace"

-- | Parser for symbol, expectation attached.
symbol :: String -> Parser String
symbol x = Token.symbol lexer x <?> x

-- | Parser for a single word (unreserved identifier).
word :: Parser String
word = Token.lexeme lexer (many1 (noneOf " \t\n\r\f\v")) <?> "word"

-- | Parser for multiple words separated by spaces.
sentence :: Parser String
sentence = Token.lexeme lexer (mconcat <$> sepBy1 word spaces) <?> "sentence"

-- | Parser for a file path (either quoted or without spaces)
filepath :: Parser FilePath
filepath = try quoted <|> unquoted <?> "file path"
    where
        quoted = Token.lexeme lexer (single <|> double) <?> "quoted file path"
        single = between (char '"') (char '"') (many (noneOf "\""))
        double = between (char '\'') (char '\'') (many (noneOf "'"))
        unquoted = Token.lexeme lexer (many1 valid) <?> "unquoted file path"
        valid = satisfy (\c -> (c `notElem` " \t\n\r\f\v") && c > '\31')
