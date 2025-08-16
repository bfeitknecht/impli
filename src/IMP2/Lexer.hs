module IMP2.Lexer where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Token as Token

-- | __TODO__
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

-- | __TODO__
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

-- | __TODO__
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

-- | __TODO__
identifier :: Parser String
identifier = Token.identifier lexer <?> "identifier"

-- | __TODO__
variable :: Parser String
variable = identifier <|> symbol "_" <?> "variable"

-- | __TODO__
reserved :: String -> Parser ()
reserved = Token.reserved lexer

-- | __TODO__
keyword :: String -> Parser ()
keyword x = reserved x <?> x

-- | __TODO__
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- | __TODO__
operator :: String -> Parser ()
operator x = reservedOp x <?> x

-- | __TODO__
parens :: Parser a -> Parser a
parens = Token.parens lexer

-- | __TODO__
integer :: Parser Integer
integer = Token.integer lexer <?> "integer"

-- | __TODO__
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer <?> "whitespace"

-- | __TODO__
symbol :: String -> Parser String
symbol x = Token.symbol lexer x <?> x
