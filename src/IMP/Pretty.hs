module IMP.Pretty where

import Data.List (intercalate)

import IMP.Syntax

class Pretty a where
    pretty :: a -> String

data Proc = Proc ([Ident], [Ident]) Stm deriving (Eq, Show)
instance Pretty Proc where
    pretty (Proc (params, rets) body) =
        "("
            ++ commas params
            ++ semicommas rets
            ++ "): " --  why the long face?
            ++ pretty body -- when you look like that?

instance Pretty Stm where
    pretty stm = case stm of
        Skip -> "skip"
        Print e -> "print " ++ pretty e
        VarDef x e -> x ++ " := " ++ pretty e
        Seq s1 s2 -> pretty s1 ++ "; " ++ pretty s2
        If b s1 s2 ->
            "if "
                ++ pretty b
                ++ " then "
                ++ pretty s1
                ++ " else "
                ++ pretty s2
                ++ " end"
        While b s ->
            "while "
                ++ pretty b
                ++ " do "
                ++ pretty s
                ++ " end"
        Local x e s ->
            "var "
                ++ x
                ++ " := "
                ++ pretty e
                ++ " in "
                ++ pretty s
                ++ " end"
        Par s1 s2 -> pretty s1 ++ " par " ++ pretty s2
        NonDet s1 s2 -> pretty s1 ++ " || " ++ pretty s2
        ProcDef name (params, rets) body ->
            "procedure "
                ++ name
                ++ "("
                ++ commas params
                ++ semicommas rets
                ++ ") "
                ++ "begin "
                ++ pretty body
                ++ " end"
        ProcInvoc name (args, rets) ->
            name
                ++ "("
                ++ commas (map pretty args)
                ++ semicommas rets
                ++ ")"

instance Pretty Aexp where
    pretty e = case e of
        Numeral n -> show n
        Variable x -> x
        Bin Add e1 e2 -> pretty e1 ++ " + " ++ pretty e2
        Bin Sub e1 e2 -> pretty e1 ++ " - " ++ pretty e2
        Bin Mul e1 e2 -> pretty e1 ++ " * " ++ pretty e2
        Time s -> "time " ++ pretty s

instance Pretty Bexp where
    pretty b = case b of
        Boolean bool -> if bool then "true" else "false"
        Not b' -> "not " ++ pretty b'
        Or b1 b2 -> pretty b1 ++ " or " ++ pretty b2
        And b1 b2 -> pretty b1 ++ " and " ++ pretty b2
        Rel Eq e1 e2 -> pretty e1 ++ " = " ++ pretty e2
        Rel Neq e1 e2 -> pretty e1 ++ " # " ++ pretty e2
        Rel Lt e1 e2 -> pretty e1 ++ " < " ++ pretty e2
        Rel Leq e1 e2 -> pretty e1 ++ " <= " ++ pretty e2
        Rel Gt e1 e2 -> pretty e1 ++ " > " ++ pretty e2
        Rel Geq e1 e2 -> pretty e1 ++ " >= " ++ pretty e2

commas :: [String] -> String
commas = intercalate ", "

semicommas :: [String] -> String
semicommas rets = ";" ++ (if null rets then "" else " ") ++ commas rets
