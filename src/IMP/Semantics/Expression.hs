{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      : IMP.Semantics.Expression
Description : Evaluation of arithmetic and boolean expressions in the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the evaluation semantics for arithmetic and boolean expressions.
-}
module IMP.Semantics.Expression where

import IMP.Semantics.State
import IMP.Syntax

-- | Typeclass for evaluating expressions or statements in a state.
class Evaluate a b | a -> b where
    -- | Evaluate a value of type @a@ in the given state, producing a result of type @b@.
    evaluate :: State -> a -> b

-- | Evaluate an arithmetic expression to an integer.
instance Evaluate Aexp Integer where
    evaluate state aexp = case aexp of
        Numeral n -> n
        Variable x -> getVar state x
        Bin aop e1 e2 ->
            let
                v1 = evaluate state e1
                v2 = evaluate state e2
            in
                case aop of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 // v2
                    Mod -> v1 %% v2
        Time s -> evaluate state s

-- | Evaluate a boolean expression to a boolean value.
instance Evaluate Bexp Bool where
    evaluate state bexp = case bexp of
        Boolean b -> b
        Or b1 b2 -> evaluate state b1 || evaluate state b2
        And b1 b2 -> evaluate state b1 && evaluate state b2
        Not b -> not (evaluate state b)
        Rel rop e1 e2 ->
            let
                v1 = evaluate state e1
                v2 = evaluate state e2
            in
                case rop of
                    Eq -> v1 == v2
                    Neq -> v1 /= v2
                    Lt -> v1 < v2
                    Leq -> v1 <= v2
                    Gt -> v1 > v2
                    Geq -> v1 >= v2

-- | Evaluate a statement to an integer (number of variable definitions).
instance Evaluate Stm Integer where
    evaluate state stm = case stm of
        Skip -> 0
        VarDef _ _ _ -> 1
        Seq s1 s2 -> evaluate state s1 + evaluate state s2
        If b s1 s2 ->
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
            Just (Proc _ _ body) -> evaluate state body + (toInteger . length) args
            Nothing -> 0
        Break -> 0
        Revert s _ -> evaluate state s
        Match e ms d -> do
            let v = evaluate state e
            case lookup v ms of
                Just s -> evaluate state s
                Nothing -> evaluate state d
        Havoc _ -> 1
        Assert _ -> 0
        Flip i s1 s2 ->
            if getFlip state i
                then evaluate state s1
                else evaluate state s2
        Raise _ -> 0
        Try s1 _ s2 -> max (evaluate state s1) (evaluate state s2 + 1)
        Swap _ _ -> 2
        Timeout s e -> min (evaluate state s) (evaluate state e)
        Alternate s1 s2 -> evaluate state s1 + evaluate state s2
        _ -> undefined

-- | Safe integer division: returns zero if divisor is zero.
(//) :: Integer -> Integer -> Integer
(//) v1 v2 = if v2 == 0 then 0 else div v1 v2

-- | Safe integer modulo: returns the dividend if divisor is zero.
(%%) :: Integer -> Integer -> Integer
(%%) v1 v2 = if v2 == 0 then v1 else mod v1 v2
