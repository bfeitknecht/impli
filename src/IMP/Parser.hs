{-# LANGUAGE TypeApplications #-}

{- |
Module      : IMP.Parser
Description : Parsing functionality for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.Parser (
    Parses,
    parses,
    parser,
) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String

import Config
import IMP.Lexer
import IMP.Syntax

-- | TODO
parser :: (Parses a) => String -> String -> Either ParseError a
parser = parse (whitespace *> parses <* eof)

-- | TODO
class Parses a where
    -- | TODO
    parses :: Parser a

-- | TODO
instance Parses Aexp where
    parses = buildExpressionParser table term <?> "arithmetic expression"
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
                    [ parens (parses @Aexp) <?> "parenthesized arithmetic expression"
                    , Val <$> integer
                    , Var <$> identifier
                    , Time <$ keyword "time" <*> parses @Stm
                    ]

-- | TODO
instance Parses Bexp where
    parses = buildExpressionParser table term <?> "boolean expression"
        where
            table =
                [ [Prefix (Not <$ operator "not")]
                , [Infix (And <$ operator "and") AssocLeft]
                , [Infix (Or <$ operator "or") AssocLeft]
                ]
            term =
                choice
                    [ parens (parses @Bexp) <?> "parenthesized boolean expression"
                    , flip Rel <$> parses @Aexp <*> parses @Rop <*> parses @Aexp <?> "relation"
                    , Lit True <$ keyword "true"
                    , Lit False <$ keyword "false"
                    ]

-- | TODO
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

-- | TODO
instance Parses Dop where
    parses =
        choice
            [ Def <$ operator ":="
            , Inc <$ operator "+="
            , Dec <$ operator "-="
            , Prod <$ operator "*="
            , Quot <$ operator "/="
            , Rem <$ operator "%="
            ]

-- | TODO
instance Parses Stm where
    parses = buildExpressionParser table term <?> "statement"
        where
            table =
                if extensions
                    then
                        [ [Infix (NonDet <$ operator "[]") AssocLeft]
                        , [Infix (Par <$ operator "par") AssocLeft]
                        , [Infix (Alternate <$ operator "alternate") AssocLeft]
                        , [Infix (Seq <$ operator ";") AssocLeft]
                        ]
                    else [[Infix (Seq <$ operator ";") AssocLeft]]
            term =
                choice . map try $
                    if extensions then imp ++ ext else imp

imp :: [Parser Stm]
imp =
    [ parens (parses @Stm) <?> "parenthesized statement"
    , Skip <$ keyword "skip"
    , VarDef <$> variable <*> parses @Dop <*> parses @Aexp
    , IfElse
        <$ keyword "if"
        <*> parses @Bexp
        <* keyword "then"
        <*> parses @Stm
        <*> option Skip (keyword "else" *> parses @Stm)
        <* keyword "end"
    , While
        <$ keyword "while"
        <*> parses @Bexp
        <* keyword "do"
        <*> parses @Stm
        <* keyword "end"
    , Print <$ keyword "print" <*> parses @Aexp
    , Read <$ keyword "read" <*> identifier
    ]

ext :: [Parser Stm]
ext =
    [ Local
        <$ keyword "var"
        <*> identifier
        <* operator ":="
        <*> parses @Aexp
        <* keyword "in"
        <*> parses @Stm
        <* keyword "end"
    , fmap ProcDef $
        Procedure
            <$ keyword "procedure"
            <*> identifier
            <*> parens (signature identifier identifier)
            <* keyword "begin"
            <*> parses @Stm
            <* keyword "end"
    , ProcInvoc
        <$> identifier
        <*> parens (signature (parses @Aexp) variable) -- allow placeholder in return variables
    , (\s b -> Seq s $ While (Not b) s)
        <$ keyword "repeat"
        <*> parses @Stm
        <* keyword "until"
        <*> parses @Bexp
    , for
        <$ keyword "for"
        <*> identifier
        <* operator ":="
        <*> parses @Aexp
        <* keyword "to"
        <*> parses @Aexp
        <* keyword "do"
        <*> parses @Stm
        <* keyword "end"
    , for "_times" (Val 0) -- unassignable counter variable prevents modification from body
        <$ keyword "do"
        <*> parses @Aexp
        <* keyword "times"
        <*> parses @Stm
    , Revert
        <$ keyword "revert"
        <*> parses @Stm
        <* keyword "if"
        <*> parses @Bexp
    , Break <$ keyword "break" -- INFO: parses outside while
    , Match
        <$ keyword "match"
        <*> parses @Aexp
        <* keyword "on"
        <*> many branch
        <* keyword "default"
        <* symbol ":"
        <*> parses @Stm
        <* keyword "end"
    , Havoc <$ keyword "havoc" <*> identifier
    , Assert <$ keyword "assert" <*> parses @Bexp
    , FlipFlop
        <$ keyword "flip"
        <*> parens integer
        <*> parses @Stm
        <* keyword "flop"
        <*> parses @Stm
        <* keyword "end"
    , Raise <$ keyword "raise" <*> parses @Aexp
    , TryCatch
        <$ keyword "try"
        <*> parses @Stm
        <* keyword "catch"
        <*> identifier
        <* keyword "with"
        <*> parses @Stm
        <* keyword "end"
    , Swap
        <$ keyword "swap"
        <*> identifier
        <*> identifier
    , Timeout
        <$ keyword "timeout"
        <*> parses @Stm
        <* keyword "after"
        <*> parses @Aexp
        <* keyword "end"
    ]

-- | TODO
instance Parses Construct where
    parses =
        choice . map try $
            [ Statement <$> parses @Stm
            , Boolean <$> parses @Bexp
            , Arithmetic <$> parses @Aexp
            , Whitespace <$ whitespace
            ]

-- | TODO
for :: String -> Aexp -> Aexp -> Stm -> Stm
for x a1 a2 s =
    Local x a1 $
        While
            (Rel Lt (Var x) a2) -- stop condition is evaluated every iteration
            (Seq s $ VarDef x Inc (Val 1))

signature :: Parser a -> Parser b -> Parser ([a], [b])
signature p1 p2 =
    (,)
        <$> sepBy p1 (symbol ",")
        <* symbol ";"
        <*> sepBy p2 (symbol ",")

-- | TODO
branch :: Parser (Integer, Stm)
branch =
    (,)
        <$> integer
        <* symbol ":"
        <*> parses @Stm
        <* symbol ","
