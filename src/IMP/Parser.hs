{- |
Module      : IMP.Parser
Description : Parsing functionality for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides parsers for the constructs of IMP,
namely arithmetic and boolean expressions and statements.
-}
module IMP.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import IMP.Syntax
import IMP.Util

class Parses a where
    parses :: Parser a

instance Parses Aexp where
    parses = buildExpressionParser table term
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
                    [ parens parses
                    , Numeral <$> integer
                    , Variable <$> identifier
                    , Time <$ reserved "time" <*> parses
                    ]

instance Parses Bexp where
    parses = buildExpressionParser table term
        where
            table =
                [ [Prefix (Not <$ operator "not")]
                , [Infix (And <$ operator "and") AssocLeft]
                , [Infix (Or <$ operator "or") AssocLeft]
                ]
            term =
                choice
                    [ parens parses
                    , flip Rel <$> parses <*> parses <*> parses
                    , Boolean True <$ reserved "true"
                    , Boolean False <$ reserved "false"
                    ]

instance Parses Rop where
    parses =
        choice
            [ Eq <$ operator "="
            , Neq <$ operator "#"
            , Leq <$ operator "<="
            , Lt <$ operator "<"
            , Geq <$ operator ">="
            , Gt <$ operator ">"
            ]

instance Parses Stm where
    parses = buildExpressionParser table term
        where
            table =
                [ [Infix (NonDet <$ reserved "[]") AssocLeft]
                , [Infix (Par <$ reserved "par") AssocLeft]
                , [Infix (Seq <$ semi) AssocLeft]
                ]
            term =
                choice . map try $
                    [ parens parses
                    , Skip <$ reserved "skip"
                    , VarDef <$> variable <*> parses <*> parses
                    , If
                        <$ reserved "if"
                        <*> parses
                        <* reserved "then"
                        <*> parses
                        <*> option Skip (reserved "else" *> parses)
                        <* reserved "end"
                    , While
                        <$ reserved "while"
                        <*> parses
                        <* reserved "do"
                        <*> parses
                        <* reserved "end"
                    , Print <$ reserved "print" <*> parses
                    , Read
                        <$ reserved "read"
                        <*> identifier
                    , Local
                        <$ reserved "var"
                        <*> variable
                        <* operator ":="
                        <*> parses
                        <* reserved "in"
                        <*> parses
                        <* reserved "end"
                    , fmap ProcDef $
                        Proc
                            <$ reserved "procedure"
                            <*> identifier
                            <*> paramret
                            <* reserved "begin"
                            <*> parses
                            <* reserved "end"
                    , ProcInvoc
                        <$> identifier
                        <*> argret
                    , (\s b -> Seq s $ While (Not b) s)
                        <$ reserved "repeat"
                        <*> parses
                        <* reserved "until"
                        <*> parses
                    , forto
                        <$ reserved "for"
                        <*> identifier
                        <* operator ":="
                        <*> parses
                        <* reserved "to"
                        <*> parses
                        <* reserved "do"
                        <*> parses
                        <* reserved "end"
                    , forto "times" (Numeral 0) -- unassignable counter variable prevents modification from body
                        <$ reserved "do"
                        <*> parses
                        <* reserved "times"
                        <*> parses
                    , Revert
                        <$ reserved "revert"
                        <*> parses
                        <* reserved "if"
                        <*> parses
                    , Break <$ reserved "break" -- also parses outside while
                    , Match
                        <$ reserved "match"
                        <*> parses
                        <* reserved "on"
                        <*> many branch
                        <* reserved "default"
                        <* symbol ":"
                        <*> parses
                        <* reserved "end"
                    , Havoc <$ reserved "havoc" <*> identifier
                    , Assert <$ reserved "assert" <*> parses
                    , Flip
                        <$ reserved "flip"
                        <*> parens integer
                        <*> parses
                        <* reserved "flop"
                        <*> parses
                        <* reserved "end"
                    , Raise <$ reserved "raise" <*> parses
                    , Try
                        <$ reserved "try"
                        <*> parses
                        <* reserved "catch"
                        <*> identifier
                        <* reserved "with"
                        <*> parses
                        <* reserved "end"
                    , Swap
                        <$ reserved "swap"
                        <*> identifier
                        <*> identifier
                    ]

instance Parses Dop where
    parses =
        choice
            [ Id <$ operator ":="
            , Inc <$ operator "+="
            , Dec <$ operator "-="
            , Prod <$ operator "*="
            , Quot <$ operator "/="
            , Rem <$ operator "%="
            ]

instance Parses Construct where
    parses =
        choice . map try $
            [ Statement <$> parses
            , Bool <$> parses
            , Arithm <$> parses
            , Whitespace <$ whitespace
            ]

forto :: String -> Aexp -> Aexp -> Stm -> Stm
forto x e1 e2 s =
    Local x e1 $
        -- stop condition is evaluated every iteration
        While
            (Rel Lt (Variable x) e2)
            (Seq s $ VarDef x Inc (Numeral 1))

paramret :: Parser ([String], [String])
paramret =
    parens $
        (,)
            <$> sepBy identifier (symbol ",")
            <* semi
            <*> sepBy identifier (symbol ",")

argret :: Parser ([Aexp], [String])
argret =
    parens $
        (,)
            <$> sepBy parses (symbol ",")
            <* semi
            <*> sepBy variable (symbol ",") -- allow placeholders in returns

branch :: Parser (Integer, Stm)
branch =
    (,)
        <$> integer
        <* symbol ":"
        <*> parses
        <* symbol ","

wrap :: String -> String
wrap = ('<' :) . (++ ">") -- this is why haskell is awesome

parser :: (Parses a) => String -> String -> Either ParseError a
parser channel = parse (whitespace *> parses <* eof) $ wrap channel
