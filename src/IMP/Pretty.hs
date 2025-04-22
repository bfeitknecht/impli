module IMP.Pretty where

import Data.Char (toLower)
import Data.List (intercalate)

import IMP.Syntax

data Proc = Proc [Var] [Var] Stm

data Construct
    = Statement Stm
    | Arithm Aexp
    | Bool Bexp

class Pretty a where
    pretty :: a -> String

instance Pretty Stm where
    pretty stm = case stm of
        Skip -> "skip"
        Print e -> "print " ++ pretty e
        VarDef x e -> x ++ " := " ++ pretty e
        Seq s1 s2 -> pretty s1 ++ "; " ++ pretty s2
        If b s1 s2 -> "if " ++ pretty b ++ " then " ++ pretty s1 ++ " else " ++ pretty s2
        While b s -> "while " ++ pretty b ++ " do " ++ pretty s
        Local x e s -> "var " ++ x ++ " := " ++ pretty e ++ " in " ++ pretty s ++ " end"
        Par s1 s2 -> pretty s1 ++ " par " ++ pretty s2
        NonDet s1 s2 -> pretty s1 ++ " || " ++ pretty s2
        ProcDef name params rets body ->
            "procedure "
                ++ name
                ++ "("
                ++ commas params
                ++ ";"
                ++ (returns rets)
                ++ ") "
                ++ "begin "
                ++ pretty body
                ++ " end"
        ProcInvoc name args rets -> name ++ "(" ++ commas (map pretty args) ++ ";" ++ (returns rets) ++ ")"
        where
            commas = intercalate ", "
            returns rets = (if null rets then "" else " ") ++ commas rets

instance Pretty Aexp where
    pretty e = case e of
        Numeral n -> show n
        Variable x -> x
        Bin Add e1 e2 -> pretty e1 ++ " + " ++ pretty e2
        Bin Sub e1 e2 -> pretty e1 ++ " - " ++ pretty e2
        Bin Mul e1 e2 -> pretty e1 ++ " * " ++ pretty e2

instance Pretty Bexp where
    pretty b = case b of
        Or b1 b2 -> pretty b1 ++ " or " ++ pretty b2
        And b1 b2 -> pretty b1 ++ " and " ++ pretty b2
        Not b -> "not " ++ pretty b
        Rel Eq e1 e2 -> pretty e1 ++ " = " ++ pretty e2
        Rel Neq e1 e2 -> pretty e1 ++ " # " ++ pretty e2
        Rel Lt e1 e2 -> pretty e1 ++ " < " ++ pretty e2
        Rel Leq e1 e2 -> pretty e1 ++ " <= " ++ pretty e2
        Rel Gt e1 e2 -> pretty e1 ++ " > " ++ pretty e2
        Rel Geq e1 e2 -> pretty e1 ++ " >= " ++ pretty e2
        Boolean bool -> map toLower $ show bool

-- replace with procdef
instance Show Proc where
    show (Proc params rets body) =
        "("
            ++ commas params
            ++ ";"
            ++ (if null rets then "" else " ")
            ++ commas rets
            ++ "): "
            ++ show body
        where
            commas = intercalate ", "

instance Show Construct where
    show construct = case construct of
        Statement s -> show s
        Arithm e -> show e
        Bool b -> show b
