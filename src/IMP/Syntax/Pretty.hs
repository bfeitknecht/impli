module IMP.Syntax.Pretty where

import Data.List (intercalate)

import IMP.Syntax.Types

class Pretty a where
    pretty :: a -> String

instance Pretty Proc where
    pretty (Proc name (params, rets) body) =
        name
            ++ "("
            ++ commas params
            ++ semicommas rets
            ++ "): " --  why the long face?
            ++ pretty body -- when you look like that?

instance Pretty Dop where
    pretty f = case f of
        Id -> " := "
        Inc -> "+="
        Dec -> "-="
        Prod -> "+="

instance Pretty Stm where
    pretty stm = case stm of
        Skip -> "skip"
        VarDef x f e -> x ++ pretty f ++ pretty e
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
        Print e -> "print " ++ pretty e
        Read x -> "read " ++ x
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
        ProcDef (Proc name (params, rets) body) ->
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
        Break -> "break"
        Revert s b ->
            "revert "
                ++ pretty s
                ++ " if "
                ++ pretty b
        Match e ms d ->
            "case "
                ++ pretty e
                ++ " of "
                ++ concatMap (\(v, s) -> show v ++ ": " ++ pretty s ++ ", ") ms
                ++ "default: "
                ++ pretty d
                ++ " end"
        Havoc x -> "havoc " ++ x
        Assert b -> "assert " ++ pretty b
        Flip i s1 s2 ->
            "flip"
                ++ "("
                ++ show i
                ++ ")"
                ++ pretty s1
                ++ " flop "
                ++ pretty s2
                ++ " end"
        Raise e -> "raise " ++ pretty e
        Try s1 x s2 ->
            "try"
                ++ pretty s1
                ++ " catch "
                ++ x
                ++ " with "
                ++ pretty s2
                ++ " end"

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
        Not b1 -> "not " ++ pretty b1
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
