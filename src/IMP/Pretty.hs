{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : IMP.Pretty
Description : Pretty-printing for IMP language constructs
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides pretty-printing functionality for IMP constructs.
-}
module IMP.Pretty where

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import IMP.Syntax

-- | Prettyprint instance for arithmetic expressions.
instance Pretty Aexp where
    pretty e = case e of
        Numeral n -> pretty n
        Variable x -> pretty x
        Bin Add e1 e2 -> pretty e1 <+> pretty "+" <+> pretty e2
        Bin Sub e1 e2 -> pretty e1 <+> pretty "-" <+> pretty e2
        Bin Mul e1 e2 -> pretty e1 <+> pretty "*" <+> pretty e2
        Bin Div e1 e2 -> pretty e1 <+> pretty "/" <+> pretty e2
        Bin Mod e1 e2 -> pretty e1 <+> pretty "%" <+> pretty e2
        Time s -> pretty "time" <+> pretty s

-- | Prettyprint instance for boolean expressions.
instance Pretty Bexp where
    pretty b = case b of
        Boolean bool -> pretty (if bool then "true" else "false")
        Not b1 -> pretty "not" <+> pretty b1
        Or b1 b2 -> pretty b1 <+> pretty "or" <+> pretty b2
        And b1 b2 -> pretty b1 <+> pretty "and" <+> pretty b2
        Rel Eq e1 e2 -> pretty e1 <+> pretty "=" <+> pretty e2
        Rel Neq e1 e2 -> pretty e1 <+> pretty "#" <+> pretty e2
        Rel Lt e1 e2 -> pretty e1 <+> pretty "<" <+> pretty e2
        Rel Leq e1 e2 -> pretty e1 <+> pretty "<=" <+> pretty e2
        Rel Gt e1 e2 -> pretty e1 <+> pretty ">" <+> pretty e2
        Rel Geq e1 e2 -> pretty e1 <+> pretty ">=" <+> pretty e2

-- | Prettyprint instance for assignment operators.
instance Pretty Dop where
    pretty f = case f of
        Def -> pretty ":="
        Inc -> pretty "+="
        Dec -> pretty "-="
        Prod -> pretty "*="
        Quot -> pretty "/="
        Rem -> pretty "%="

-- | Prettyprint instance for statements.
instance Pretty Stm where
    pretty stm = case stm of
        Skip -> pretty "skip"
        VarDef x f e -> pretty x <+> pretty f <+> pretty e
        Seq s1 s2 -> vsep [pretty s1 <> semi, pretty s2]
        If b s1 s2 ->
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
        Print e -> pretty "print" <+> pretty e
        Read x -> pretty "read" <+> pretty x
        Local x e s ->
            vsep
                [ pretty "var" <+> pretty x <+> pretty ":=" <+> pretty e <+> pretty "in"
                , indent 4 (pretty s)
                , pretty "end"
                ]
        Par s1 s2 -> pretty s1 <+> pretty "par" <+> pretty s2
        NonDet s1 s2 -> pretty s1 <+> pretty "[]" <+> pretty s2
        ProcDef (Proc name (params, rets) body) ->
            vsep
                [ pretty "procedure" <+> pretty name <> parens (semmicommas params rets)
                , pretty "begin"
                , indent 4 (pretty body)
                , pretty "end"
                ]
        ProcInvoc name (args, rets) ->
            pretty name <> parens (semmicommas args rets)
        Break -> pretty "break"
        Revert s b ->
            vsep
                [ pretty "revert" <+> pretty s <+> pretty "if" <+> pretty b
                ]
        Match e ms d ->
            vsep
                [ pretty "match" <+> pretty e <+> pretty "on"
                , indent 4 $
                    vsep $
                        map (\(v, s) -> pretty v <> colon <+> pretty s <> comma) ms
                            ++ [pretty "default:" <+> pretty d]
                , pretty "end"
                ]
        Havoc x -> pretty "havoc" <+> pretty x
        Assert b -> pretty "assert" <+> pretty b
        Flip i s1 s2 ->
            vsep
                [ pretty "flip" <> parens (pretty i)
                , indent 4 (pretty s1)
                , pretty "flop"
                , indent 4 (pretty s2)
                , pretty "end"
                ]
        Raise e -> pretty "raise" <+> pretty e
        Try s1 x s2 ->
            vsep
                [ pretty "try"
                , indent 4 (pretty s1)
                , pretty "catch" <+> pretty x <+> pretty "with"
                , indent 4 (pretty s2)
                , pretty "end"
                ]
        Swap x y -> pretty "swap" <+> pretty x <+> pretty y
        Timeout s e ->
            vsep
                [ pretty "timeout"
                , indent 4 (pretty s)
                , pretty "after" <+> pretty e
                , pretty "end"
                ]
        Alternate s1 s2 ->
            vsep
                [ pretty s1
                , indent 4 $ pretty "alternate"
                , pretty s2
                ]
        _ -> undefined

-- | Prettyprint instance for procedures.
instance Pretty Proc where
    pretty (Proc name (params, rets) body) =
        vsep
            [ pretty name <> parens (semmicommas params rets) <> colon
            , indent 4 (pretty body)
            ]

-- | Render prettyprintable value to a string with layout.
prettify :: (Pretty a) => a -> String
prettify = renderString . layoutPretty defaultLayoutOptions . pretty

-- | Render prettyprintable value to a single-line string.
stringify :: (Pretty a) => a -> String
stringify = unwords . words . prettify

-- | Combine documents with commas.
commas :: [Doc ann] -> Doc ann
commas = hsep . punctuate comma

-- | Combine two lists of prettyprintable values with semicolon separator.
semmicommas :: (Pretty a, Pretty b) => [a] -> [b] -> Doc ann
semmicommas xs ys = commas (map pretty xs) <> semi <+> commas (map pretty ys)
