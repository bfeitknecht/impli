{-# LANGUAGE TypeApplications #-}

{- |
Module      : IMP.Parser
Description : Parsing functionality for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides parsers for the constructs defined in "IMP.Syntax".
For this it utilizes the "Text.Parsec" library and exposes the 'Parse' typeclass and 'parse' function.
Additionally, the 'parser' function serves as top level abstraction used in "IMP.REPL" and "IMP.CLI".
-}
module IMP.Parser (
    Parse,
    parse,
    parser,
) where

import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Prim as Parsec

import IMP.Syntax
import IMP.Util

-- | Typeclass for "IMP.Syntax" data types that can be parsed from source code string
class Parse a where
    -- | The parser for the type.
    parse :: Parser a

-- | Parser for arithmetic expressions, 'Aexp'.
-- Supports numerals, variables, arithmetic operations, and the @time@ operation.
instance Parse Aexp where
    parse = buildExpressionParser table term <?> "arithmetic expression"
        where
            table =
                [
                    [ Infix (Bin Mul <$ operator "*") AssocLeft
                    , Infix (Bin Div <$ operator "/") AssocLeft
                    , Infix (Bin Mod <$ operator "%") AssocLeft
                    ]
                ,
                    [ Infix (Bin Add <$ operator "+") AssocLeft
                    , Infix (Bin Sub <$ operator "-") AssocLeft
                    ]
                ]
            term =
                choice
                    [ parens (parse @Aexp) <?> "parenthesized arithmetic expression"
                    , Val <$> integer
                    , Var <$> identifier
                    , Time <$ keyword "time" <*> parse @Stm
                    ]

-- | Parser for boolean expressions, 'Bexp'.
-- Supports boolean literals, boolean operations, and relational expressions.
instance Parse Bexp where
    parse = buildExpressionParser table term <?> "boolean expression"
        where
            table =
                [ [Prefix (Not <$ operator "not")]
                , [Infix (And <$ operator "and") AssocLeft]
                , [Infix (Or <$ operator "or") AssocLeft]
                ]
            relation = flip Rel <$> parse @Aexp <*> parse @Rop <*> parse @Aexp
            term =
                choice
                    [ parens (parse @Bexp) <?> "parenthesized boolean expression"
                    , relation <?> "relation"
                    , Lit True <$ keyword "true"
                    , Lit False <$ keyword "false"
                    ]

-- | Parser for relational operators, 'Rop'.
-- Supports equality, inequality, and comparison operators.
instance Parse Rop where
    parse =
        choice
            [ Eq <$ operator "="
            , Neq <$ operator "#"
            , Leq <$ operator "<="
            , Lt <$ operator "<"
            , Geq <$ operator ">="
            , Gt <$ operator ">"
            ]

-- | Parser for statements, 'Stm'. Supports all IMP language statements.
instance Parse Stm where
    parse = buildExpressionParser table term <?> "statement"
        where
            table =
                [ [Infix (NonDet <$ keyword "[]") AssocLeft]
                , [Infix (Par <$ keyword "par") AssocLeft]
                , [Infix (Alternate <$ keyword "alternate") AssocLeft]
                , [Infix (Seq <$ symbol ";") AssocLeft]
                ]
            term =
                choice . map try $
                    [ parens (parse @Stm) <?> "parenthesized statement"
                    , Skip <$ keyword "skip"
                    , VarDef <$> variable <*> parse @Dop <*> parse @Aexp
                    , If
                        <$ keyword "if"
                        <*> parse @Bexp
                        <* keyword "then"
                        <*> parse @Stm
                        <*> option Skip (keyword "else" *> parse @Stm)
                        <* keyword "end"
                    , While
                        <$ keyword "while"
                        <*> parse @Bexp
                        <* keyword "do"
                        <*> parse @Stm
                        <* keyword "end"
                    , Print <$ keyword "print" <*> parse @Aexp
                    , Read
                        <$ keyword "read"
                        <*> identifier
                    , Local
                        <$ keyword "var"
                        <*> identifier
                        <* operator ":="
                        <*> parse @Aexp
                        <* keyword "in"
                        <*> parse @Stm
                        <* keyword "end"
                    , fmap ProcDef $
                        Procedure
                            <$ keyword "procedure"
                            <*> identifier
                            <*> paramret
                            <* keyword "begin"
                            <*> parse @Stm
                            <* keyword "end"
                    , ProcInvoc
                        <$> identifier
                        <*> argret
                    , (\s b -> Seq s $ While (Not b) s)
                        <$ keyword "repeat"
                        <*> parse @Stm
                        <* keyword "until"
                        <*> parse @Bexp
                    , forto
                        <$ keyword "for"
                        <*> identifier
                        <* operator ":="
                        <*> parse @Aexp
                        <* keyword "to"
                        <*> parse @Aexp
                        <* keyword "do"
                        <*> parse @Stm
                        <* keyword "end"
                    , forto "_times" (Val 0) -- unassignable counter variable prevents modification from body
                        <$ keyword "do"
                        <*> parse @Aexp
                        <* keyword "times"
                        <*> parse @Stm
                    , Revert
                        <$ keyword "revert"
                        <*> parse @Stm
                        <* keyword "if"
                        <*> parse @Bexp
                    , Break <$ keyword "break" -- also parse outside while
                    , Match
                        <$ keyword "match"
                        <*> parse @Aexp
                        <* keyword "on"
                        <*> many branch
                        <* keyword "default"
                        <* symbol ":"
                        <*> parse @Stm
                        <* keyword "end"
                    , Havoc <$ keyword "havoc" <*> identifier
                    , Assert <$ keyword "assert" <*> parse @Bexp
                    , Flip
                        <$ keyword "flip"
                        <*> parens integer
                        <*> parse @Stm
                        <* keyword "flop"
                        <*> parse @Stm
                        <* keyword "end"
                    , Raise <$ keyword "raise" <*> parse @Aexp
                    , Try
                        <$ keyword "try"
                        <*> parse @Stm
                        <* keyword "catch"
                        <*> identifier
                        <* keyword "with"
                        <*> parse @Stm
                        <* keyword "end"
                    , Swap
                        <$ keyword "swap"
                        <*> identifier
                        <*> identifier
                    , Timeout
                        <$ keyword "timeout"
                        <*> parse @Stm
                        <* keyword "after"
                        <*> parse @Aexp
                        <* keyword "end"
                    ]

-- | Parser for variable definition operators, 'Dop'.
-- Supports simple, increment, decrement, product, quotient, and remainder definitions.
instance Parse Dop where
    parse =
        choice
            [ Def <$ operator ":="
            , Inc <$ operator "+="
            , Dec <$ operator "-="
            , Prod <$ operator "*="
            , Quot <$ operator "/="
            , Rem <$ operator "%="
            ]

-- | Parser for IMP language constructs, 'Construct' which encapsulate
-- arithmetic and boolean expressions, statements and whitespace.
instance Parse Construct where
    parse =
        choice . map try $
            [ Statement <$> parse @Stm
            , Boolean <$> parse @Bexp
            , Arithmetic <$> parse @Aexp
            , Whitespace <$ whitespace
            ]

-- | Constructor for bounded loop.
forto :: String -> Aexp -> Aexp -> Stm -> Stm
forto x e1 e2 s =
    Local x e1 $
        -- stop condition is evaluated every iteration
        While
            (Rel Lt (Var x) e2)
            (Seq s $ VarDef x Inc (Val 1))

-- | Parser for procedure parameter and return variable lists.
paramret :: Parser ([String], [String])
paramret =
    parens $
        (,)
            <$> sepBy identifier (symbol ",")
            <* symbol ";"
            <*> sepBy identifier (symbol ",")

-- | Parser for procedure argument and return variable lists.
argret :: Parser ([Aexp], [String])
argret =
    parens $
        (,)
            <$> sepBy (parse @Aexp) (symbol ",")
            <* symbol ";"
            <*> sepBy variable (symbol ",") -- allow placeholders in returns

-- | Parser for single pattern match branch.
branch :: Parser (Integer, Stm)
branch =
    (,)
        <$> integer
        <* symbol ":"
        <*> parse @Stm
        <* symbol ","

-- | Wrap string in angle brackets.
wrap :: String -> String
wrap = ('<' :) . (++ ">") -- this is why haskell is awesome

-- | Top-level parser for any type that implements 'Parse'.
-- Takes a channel name for error reporting and input string to parse,
-- and returns either parse error or the successfully parsed value.
-- Used in "IMP.REPL" to parse user input and "IMP.CLI" to parse source code.
parser :: (Parse a) => String -> String -> Either ParseError a
parser channel = Parsec.parse (whitespace *> parse <* eof) $ wrap channel
