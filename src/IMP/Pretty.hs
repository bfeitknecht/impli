{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : IMP.Pretty
Description : Pretty-printing for IMP language constructs
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Pretty-print implementation for constructs of the IMP language.
Parentheses are inserted by precedence, such that the output parses back
into the same abstract syntax.
-}
module IMP.Pretty (
    prettify,
    stringify,
    prettytrace,
) where

import Prettyprinter
import Prettyprinter.Render.String

import IMP.Syntax

-- | Pretty-print instance for arithmetic expression.
instance Pretty Aexp where
    pretty = aexp 0

-- | Pretty-print instance for boolean expression.
instance Pretty Bexp where
    pretty = bexp 0

-- | Pretty-print instance for statement.
instance Pretty Stm where
    pretty = stm 0

-- | Pretty-print instance for construct.
instance Pretty Construct where
    pretty construct = case construct of
        Statement s -> pretty s
        Arithmetic a -> pretty a
        Boolean b -> pretty b
        Whitespace -> emptyDoc

-- | Parenthesize if context binds tighter than construct.
wrap :: Int -> Int -> Doc ann -> Doc ann
wrap context precedence doc
    | context > precedence = parens doc
    | otherwise = doc

-- | Arithmetic expression in precedence context.
aexp :: Int -> Aexp -> Doc ann
aexp context a = case a of
    Val n -> pretty n
    Var x -> pretty x
    Bin aop a1 a2 ->
        let
            precedence = if aop `elem` [Add, Sub] then 1 else 2
            symbol = case aop of
                Add -> "+"
                Sub -> "-"
                Mul -> "*"
                Div -> "/"
                Mod -> "%"
        in
            wrap context precedence $ aexp precedence a1 <+> pretty symbol <+> aexp (precedence + 1) a2
    Time s -> parens (pretty "time" <+> stm 0 s) -- INFO: Statement extends as far as possible

-- | Boolean expression in precedence context.
bexp :: Int -> Bexp -> Doc ann
bexp context b = case b of
    Lit bool -> pretty (if bool then "true" else "false")
    Or b1 b2 -> wrap context 1 $ bexp 1 b1 <+> pretty "or" <+> bexp 2 b2
    And b1 b2 -> wrap context 2 $ bexp 2 b1 <+> pretty "and" <+> bexp 3 b2
    Not b' -> wrap context 3 $ pretty "not" <+> bexp 4 b' -- INFO: Prefix operator doesn't nest
    Rel rop a1 a2 ->
        let symbol = case rop of
                Eq -> "="
                Neq -> "#"
                Lt -> "<"
                Leq -> "<="
                Gt -> ">"
                Geq -> ">="
        in aexp 0 a1 <+> pretty symbol <+> aexp 0 a2

-- | Statement in precedence context.
stm :: Int -> Stm -> Doc ann
stm context s = case s of
    Seq s1 s2 -> wrap context 1 $ vsep [stm 2 s1 <> semi, stm 1 s2] -- INFO: Right associative
    Alternate s1 s2 -> wrap context 2 $ vsep [stm 2 s1, pretty "alternate", stm 3 s2]
    Par s1 s2 -> wrap context 3 $ stm 3 s1 <+> pretty "par" <+> stm 4 s2
    NonDet s1 s2 -> wrap context 4 $ stm 4 s1 <+> pretty "[]" <+> stm 5 s2
    Skip -> pretty "skip"
    VarDef x dop a ->
        let symbol = case dop of
                Def -> ":="
                Inc -> "+="
                Dec -> "-="
                Prod -> "*="
                Quot -> "/="
                Rem -> "%="
        in pretty x <+> pretty symbol <+> pretty a
    IfElse b s1 s2 ->
        vsep
            [ pretty "if" <+> pretty b <+> pretty "then"
            , block s1
            , pretty "else"
            , block s2
            , pretty "end"
            ]
    While b s' ->
        vsep
            [ pretty "while" <+> pretty b <+> pretty "do"
            , block s'
            , pretty "end"
            ]
    Print a -> pretty "print" <+> pretty a
    Read x -> pretty "read" <+> pretty x
    Local x a s' ->
        vsep
            [ pretty "let" <+> pretty x <+> pretty ":=" <+> pretty a <+> pretty "in"
            , block s'
            , pretty "end"
            ]
    ProcDef name params rets body ->
        vsep
            [ pretty "procedure" <+> pretty name <> parens (signature params rets)
            , pretty "begin"
            , block body
            , pretty "end"
            ]
    ProcInvoc name args rets -> pretty name <> parens (signature args rets)
    Break -> pretty "break"
    Revert s' b ->
        vsep
            [ pretty "revert"
            , block s'
            , pretty "if" <+> pretty b <+> pretty "end"
            ]
    Match a ms d ->
        vsep
            [ pretty "match" <+> pretty a <+> pretty "on"
            , indent 4 . vsep $
                [pretty v <> colon <+> pretty s' <> comma | (v, s') <- ms]
                    ++ [pretty "default:" <+> pretty d]
            , pretty "end"
            ]
    Havoc x -> pretty "havoc" <+> pretty x
    Assert b -> pretty "assert" <+> pretty b
    FlipFlop i s1 s2 ->
        vsep
            [ pretty "flip" <> parens (pretty i)
            , block s1
            , pretty "flop"
            , block s2
            , pretty "end"
            ]
    Raise a -> pretty "raise" <+> pretty a
    TryCatch s1 x s2 ->
        vsep
            [ pretty "try"
            , block s1
            , pretty "catch" <+> pretty x <+> pretty "with"
            , block s2
            , pretty "end"
            ]
    Swap x y -> pretty "swap" <+> pretty x <+> pretty y
    Timeout s' a ->
        vsep
            [ pretty "timeout"
            , block s'
            , pretty "after" <+> pretty a
            , pretty "end"
            ]
    where
        block = indent 4 . pretty

-- | Format signature with commas and semicolon separator.
signature :: (Pretty a, Pretty b) => [a] -> [b] -> Doc ann
signature xs ys = commas (map pretty xs) <> semi <+> commas (map pretty ys)
    where
        commas = hsep . punctuate comma

-- | Convert to pretty string with layout.
prettify :: (Pretty a) => a -> String
prettify = renderString . layoutPretty defaultLayoutOptions . pretty

-- | Convert to pretty single line string.
stringify :: (Pretty a) => a -> String
stringify = unwords . words . prettify

-- | Convert trace of statements to valid IMP language source code.
prettytrace :: [Stm] -> String
prettytrace = renderString . layoutPretty defaultLayoutOptions . vsep . punctuate semi . map (stm 2)
