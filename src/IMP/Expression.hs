{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      : IMP.Expression
Description : Expression evaluation for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Expression evaluation for the IMP language in the context of some state.
Statements evaluate to the count of atomic operations.
-}
module IMP.Expression (
    Evaluate,
    evaluate,
) where

import IMP.State
import IMP.Syntax

-- | Typeclass to evaluate functional dependency.
class Evaluate a b | a -> b where
    -- | Evaluate type in the context of some state to unique type.
    evaluate :: State -> a -> b

-- | Evaluate arithmetic expression to integer.
instance Evaluate Aexp Integer where
    evaluate state aexp = case aexp of
        Val n -> n
        Var x -> getVar state x
        Bin aop a1 a2 ->
            let
                v1 = evaluate state a1
                v2 = evaluate state a2
            in
                case aop of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 // v2
                    Mod -> v1 %% v2
        Time s -> evaluate state s

-- | Evaluate boolean expression to boolean.
instance Evaluate Bexp Bool where
    evaluate state bexp = case bexp of
        Lit b -> b
        Or b1 b2 -> evaluate state b1 || evaluate state b2
        And b1 b2 -> evaluate state b1 && evaluate state b2
        Not b -> not (evaluate state b)
        Rel rop a1 a2 ->
            let
                v1 = evaluate state a1
                v2 = evaluate state a2
            in
                case rop of
                    Eq -> v1 == v2
                    Neq -> v1 /= v2
                    Lt -> v1 < v2
                    Leq -> v1 <= v2
                    Gt -> v1 > v2
                    Geq -> v1 >= v2

-- | Evaluate statement to integer.
-- CHECK: rewrite to count steps in operational semantics
instance Evaluate Stm Integer where
    evaluate state stm = case stm of
        Skip -> 0
        VarDef {} -> 1
        Seq s1 s2 -> evaluate state s1 + evaluate state s2
        IfElse b s1 s2 ->
            if evaluate state b
                then evaluate state s1
                else evaluate state s2
        While b s ->
            if evaluate state b
                then evaluate state s + evaluate state stm
                else 0
        Print _ -> 0
        Read _ -> 1
        Local _ _ s -> evaluate state s + 1
        NonDet s1 s2 -> max (evaluate state s1) (evaluate state s2)
        Par s1 s2 -> evaluate state s1 + evaluate state s2
        ProcDef _ -> 0
        ProcInvoc name (args, _) -> case getProc state name of
            Just p -> evaluate state (procbody p) + (toInteger . length) args
            Nothing -> 0
        Restore _ -> error $ "illegal statement for evaluate: " ++ show stm
        Return _ _ -> error $ "illegal statement for evaluate: " ++ show stm
        Break -> 0
        Revert s _ -> evaluate state s
        Match a ms d -> do
            let v = evaluate state a
            case lookup v ms of
                Just s -> evaluate state s
                Nothing -> evaluate state d
        Havoc _ -> 1
        Assert _ -> 0
        FlipFlop i s1 s2 ->
            if getFlip state i
                then evaluate state s1
                else evaluate state s2
        Raise _ -> 0
        TryCatch s1 _ s2 -> max (evaluate state s1) (evaluate state s2 + 1)
        Swap _ _ -> 2
        Timeout s a -> min (evaluate state s) (evaluate state a)
        Alternate s1 s2 -> evaluate state s1 + evaluate state s2
