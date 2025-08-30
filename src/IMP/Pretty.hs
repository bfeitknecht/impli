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
-}
module IMP.Pretty (
    prettify,
    stringify,
) where

import Prettyprinter
import Prettyprinter.Render.String

import IMP.Syntax

-- | Pretty-print instance for arithmetic expression.
instance Pretty Aexp where
    pretty a = case a of
        Val n -> pretty n
        Var x -> pretty x
        Bin Add a1 a2 -> pretty a1 <+> pretty "+" <+> pretty a2
        Bin Sub a1 a2 -> pretty a1 <+> pretty "-" <+> pretty a2
        Bin Mul a1 a2 -> pretty a1 <+> pretty "*" <+> pretty a2
        Bin Div a1 a2 -> pretty a1 <+> pretty "/" <+> pretty a2
        Bin Mod a1 a2 -> pretty a1 <+> pretty "%" <+> pretty a2
        Time s -> pretty "time" <+> pretty s

-- | Pretty-print instance for boolean expression.
instance Pretty Bexp where
    pretty b = case b of
        Lit bool -> pretty (if bool then "true" else "false")
        Not b1 -> pretty "not" <+> pretty b1
        Or b1 b2 -> pretty b1 <+> pretty "or" <+> pretty b2
        And b1 b2 -> pretty b1 <+> pretty "and" <+> pretty b2
        Rel Eq a1 a2 -> pretty a1 <+> pretty "=" <+> pretty a2
        Rel Neq a1 a2 -> pretty a1 <+> pretty "#" <+> pretty a2
        Rel Lt a1 a2 -> pretty a1 <+> pretty "<" <+> pretty a2
        Rel Leq a1 a2 -> pretty a1 <+> pretty "<=" <+> pretty a2
        Rel Gt a1 a2 -> pretty a1 <+> pretty ">" <+> pretty a2
        Rel Geq a1 a2 -> pretty a1 <+> pretty ">=" <+> pretty a2

-- | Pretty-print instance for statement.
instance Pretty Stm where
    pretty stm = case stm of
        Skip -> pretty "skip"
        VarDef x Def a -> pretty x <+> pretty ":=" <+> pretty a
        VarDef x Inc a -> pretty x <+> pretty "+=" <+> pretty a
        VarDef x Dec a -> pretty x <+> pretty "-=" <+> pretty a
        VarDef x Prod a -> pretty x <+> pretty "*=" <+> pretty a
        VarDef x Quot a -> pretty x <+> pretty "/=" <+> pretty a
        VarDef x Rem a -> pretty x <+> pretty "%=" <+> pretty a
        Seq s1 s2 -> vsep [pretty s1 <> semi, pretty s2]
        IfElse b s1 s2 ->
            vsep
                [ pretty "if" <+> pretty b <+> pretty "then"
                , indent 4 (pretty s1)
                , pretty "else"
                , indent 4 (pretty s2)
                , pretty "end"
                ]
        While b s ->
            vsep
                [ pretty "while" <+> pretty b <+> pretty "do"
                , indent 4 (pretty s)
                , pretty "end"
                ]
        Print a -> pretty "print" <+> pretty a
        Read x -> pretty "read" <+> pretty x
        Local x a s ->
            vsep
                [ pretty "var" <+> pretty x <+> pretty ":=" <+> pretty a <+> pretty "in"
                , indent 4 (pretty s)
                , pretty "end"
                ]
        Par s1 s2 -> pretty s1 <+> pretty "par" <+> pretty s2
        NonDet s1 s2 -> pretty s1 <+> pretty "[]" <+> pretty s2
        ProcDef (Procedure name (params, rets) body) ->
            vsep
                [ pretty "procedure" <+> pretty name <> parens (semmicommas params rets)
                , pretty "begin"
                , indent 4 (pretty body)
                , pretty "end"
                ]
        ProcInvoc name (args, rets) -> pretty name <> parens (semmicommas args rets)
        Break -> pretty "break"
        Revert s b ->
            vsep
                [ pretty "revert" <+> pretty s <+> pretty "if" <+> pretty b
                ]
        Match a ms d ->
            vsep
                [ pretty "match" <+> pretty a <+> pretty "on"
                , indent 4 $
                    vsep $
                        map (\(v, s) -> pretty v <> colon <+> pretty s <> comma) ms
                            ++ [pretty "default:" <+> pretty d]
                , pretty "end"
                ]
        Havoc x -> pretty "havoc" <+> pretty x
        Assert b -> pretty "assert" <+> pretty b
        FlipFlop i s1 s2 ->
            vsep
                [ pretty "flip" <> parens (pretty i)
                , indent 4 (pretty s1)
                , pretty "flop"
                , indent 4 (pretty s2)
                , pretty "end"
                ]
        Raise a -> pretty "raise" <+> pretty a
        TryCatch s1 x s2 ->
            vsep
                [ pretty "try"
                , indent 4 (pretty s1)
                , pretty "catch" <+> pretty x <+> pretty "with"
                , indent 4 (pretty s2)
                , pretty "end"
                ]
        Swap x y -> pretty "swap" <+> pretty x <+> pretty y
        Timeout s a ->
            vsep
                [ pretty "timeout"
                , indent 4 (pretty s)
                , pretty "after" <+> pretty a
                , pretty "end"
                ]
        Alternate s1 s2 ->
            vsep
                [ pretty s1
                , indent 4 $ pretty "alternate"
                , pretty s2
                ]
        _ -> undefined

-- | Pretty-print instance for procedure definition.
instance Pretty Proc where
    pretty (Procedure name (params, rets) body) =
        vsep
            [ pretty name <> parens (semmicommas params rets) <> colon
            , indent 4 (pretty body)
            ]

-- | Convert to pretty string with layout.
prettify :: (Pretty a) => a -> String
prettify = renderString . layoutPretty defaultLayoutOptions . pretty

-- | Convert to pretty single line string.
stringify :: (Pretty a) => a -> String
stringify = unwords . words . prettify

-- | Format list of documents with comma separators.
commas :: [Doc ann] -> Doc ann
commas = hsep . punctuate comma

-- | Format signature with commas and semicolon separator.
semmicommas :: (Pretty a, Pretty b) => [a] -> [b] -> Doc ann
semmicommas xs ys = commas (map pretty xs) <> semi <+> commas (map pretty ys)
