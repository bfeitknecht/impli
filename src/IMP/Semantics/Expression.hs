{-# LANGUAGE FunctionalDependencies #-}

module IMP.Semantics.Expression where

import IMP.Semantics.State
import IMP.Syntax.Types

class Evaluates a b | a -> b where
    evaluates :: State -> a -> b

instance Evaluates Aexp Integer where
    evaluates state aexp = case aexp of
        Numeral n -> n
        Variable x -> getVar state x
        Bin op e1 e2 ->
            let
                v1 = evaluates state e1
                v2 = evaluates state e2
            in
                case op of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
        Time s -> toInteger $ evaluates state s

instance Evaluates Bexp Bool where
    evaluates state bexp = case bexp of
        Boolean b -> b
        Or b1 b2 -> evalBexp state b1 || evalBexp state b2
        And b1 b2 -> evalBexp state b1 && evalBexp state b2
        Not b -> not (evalBexp state b)
        Rel rop e1 e2 ->
            let
                v1 = evalAexp state e1
                v2 = evalAexp state e2
            in
                case rop of
                    Eq -> v1 == v2
                    Neq -> v1 /= v2
                    Lt -> v1 < v2
                    Leq -> v1 <= v2
                    Gt -> v1 > v2
                    Geq -> v1 >= v2

instance Evaluates Stm Int where
    evaluates state stm = case stm of
        Skip -> 0
        VarDef _ _ _ -> 1
        Seq s1 s2 -> evaluates state s1 + evaluates state s2
        If b s1 s2 ->
            if evalBexp state b
                then evaluates state s1
                else evaluates state s2
        While b s ->
            if evalBexp state b
                then evaluates state s
                else 0
        Print _ -> 0
        Read _ -> 1
        Local _ _ s -> evaluates state s + 1
        NonDet s1 s2 -> max (evaluates state s1) (evaluates state s2)
        Par s1 s2 -> evaluates state s1 + evaluates state s2
        ProcDef _ -> 0
        ProcInvoc name (args, _) -> case getProc state name of
            Just (Proc _ _ body) -> evaluates state body + length args
            Nothing -> 0
        Break -> 0
        Revert s b ->
            if evalBexp state b -- NOTE: should evaluate in state after executes s
                then evaluates state s
                else 0
        Match e ms d -> do
            let v = evalAexp state e
            case lookup v ms of
                Just s -> evaluates state s
                Nothing -> evaluates state d
        Havoc _ -> 1
        Assert _ -> 0
        Flip i s1 s2 ->
            if getVar state ("_" ++ show i) == 0
                then evaluates state s1
                else evaluates state s2
        Raise _ -> 0
        Try s1 _ s2 -> max (evaluates state s1) (evaluates state s2 + 1)

evalAexp :: State -> Aexp -> Integer
evalAexp state aexp = case aexp of
    Numeral n -> n
    Variable x -> getVar state x
    Bin op e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case op of
                Add -> v1 + v2
                Sub -> v1 - v2
                Mul -> v1 * v2
    Time s -> countVarDefs state s

evalBexp :: State -> Bexp -> Bool
evalBexp state bexp = case bexp of
    Boolean b -> b
    Or b1 b2 -> evalBexp state b1 || evalBexp state b2
    And b1 b2 -> evalBexp state b1 && evalBexp state b2
    Not b -> not (evalBexp state b)
    Rel rop e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case rop of
                Eq -> v1 == v2
                Neq -> v1 /= v2
                Lt -> v1 < v2
                Leq -> v1 <= v2
                Gt -> v1 > v2
                Geq -> v1 >= v2

countVarDefs :: State -> Stm -> Integer
countVarDefs state stm = case stm of
    Skip -> 0
    VarDef _ _ _ -> 1
    Seq s1 s2 -> countVarDefs state s1 + countVarDefs state s2
    If b s1 s2 ->
        if evalBexp state b
            then countVarDefs state s1
            else countVarDefs state s2
    While b s ->
        if evalBexp state b
            then countVarDefs state s
            else 0
    Print _ -> 0
    Read _ -> 1
    Local _ _ s -> countVarDefs state s + 1
    NonDet s1 s2 -> max (countVarDefs state s1) (countVarDefs state s2)
    Par s1 s2 -> countVarDefs state s1 + countVarDefs state s2
    ProcDef _ -> 0
    ProcInvoc name (args, _) -> case getProc state name of
        Just (Proc _ _ body) -> countVarDefs state body + toInteger (length args)
        Nothing -> 0
    Break -> 0
    Revert s b ->
        if evalBexp state b -- NOTE: deviation from semantic definition
            then countVarDefs state s
            else 0
    Match e ms d -> do
        let v = evalAexp state e
        case lookup v ms of
            Just s -> countVarDefs state s
            Nothing -> countVarDefs state d
    Havoc _ -> 1
    Assert _ -> 0
    Flip i s1 s2 ->
        if getVar state ("_" ++ show i) == 0
            then countVarDefs state s1
            else countVarDefs state s2
    Raise _ -> 0
    Try s1 _ s2 -> max (countVarDefs state s1) (countVarDefs state s2 + 1)
