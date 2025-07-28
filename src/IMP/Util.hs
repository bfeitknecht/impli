{- |
Module      : IMP.Util
Description : Utility functions for the IMP language parser
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides utility functions and definitions used in the
"IMP.Parser" module. It includes token parsers, reserved keywords, operators,
and helper functions for parsing identifiers, symbols, and whitespace.
It also provides safe arithmetic operations used by "IMP.Semantics.Expression"
and string manipulation utilities used throughout the interpreter.
-}
module IMP.Util where

import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

-- | Reserved keywords in the IMP language.
-- Used by "IMP.Parser" for syntax recognition.
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

-- | Reserved operators in the IMP language.
-- Used by "IMP.Parser" for operator recognition.
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

-- | Reserved meta-commands for the REPL.
-- Used by "IMP.REPL" to recognize metacommand.
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

-- | Parser for identifier (variable or procedure name).
-- Used for variable references and procedure names.
identifier :: Parser String
identifier = Tok.identifier lexer <?> "identifier"

-- | Parser for variable name or the placeholder @_@.
variable :: Parser String
variable = identifier <|> symbol "_" <?> "variable"

-- | Parser for keyword from the list of 'keywords'.
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Parser for reserved keyword and adds label to it.
-- Used by "IMP.Parser" to parse language keywords.
keyword :: String -> Parser ()
keyword x = reserved x <?> x

-- | Parser for operator from the list of 'operators'.
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Parser for reserved operator and adds label to it.
-- Used by "IMP.Parser" to parse language operators.
operator :: String -> Parser ()
operator x = reservedOp x <?> x

-- | Parser for parentheses around another parser.
-- Used for grouping expressions and parameter lists.
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Parser for integer literal.
-- Used by "IMP.Parser" to parse numerical constants.
integer :: Parser Integer
integer = Tok.integer lexer <?> "integer"

-- | Parser for whitespace, including comments.
-- Used by "IMP.Parser" to ignore whitespace in the source code.
whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer <?> "whitespace"

-- | Parser for symbol with label.
-- Used by "IMP.Parser" to parse punctuation and other non-keyword symbols.
symbol :: String -> Parser String
symbol x = Tok.symbol lexer x <?> x

-- | Unlines without final newline.
-- Used by "IMP.REPL" and "IMP.Semantics.Statement" for text formatting.
unlines' :: [String] -> String
unlines' = init . unlines

-- | Safe integer division: returns zero if divisor is zero.
--    Used by "IMP.Semantics.Expression" to implement division that doesn't raise exceptions.
(//) :: Integer -> Integer -> Integer
(//) v1 v2 = if v2 == 0 then 0 else div v1 v2

-- | Safe integer modulo: returns the dividend if divisor is zero.
--    Used by "IMP.Semantics.Expression" to implement modulo that doesn't raise exceptions.
(%%) :: Integer -> Integer -> Integer
(%%) v1 v2 = if v2 == 0 then v1 else mod v1 v2

-- | Flush stdout.
-- Used by "IMP.REPL" and "IMP.Semantics.Statement" to ensure output is displayed immediately.
flush :: IO ()
flush = hFlush stdout
