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
    evaluate :: a -> State -> b

-- | Evaluate arithmetic expression to integer.
instance Evaluate Aexp Integer where
    evaluate aexp state = case aexp of
        Val n -> n
        Var x -> getVar state x
        Bin aop a1 a2 ->
            let
                v1 = evaluate a1 state
                v2 = evaluate a2 state
            in
                case aop of
                    Add -> v1 + v2
                    Sub -> v1 - v2
                    Mul -> v1 * v2
                    Div -> v1 // v2
                    Mod -> v1 %% v2
        Time s -> evaluate s state

-- | Evaluate boolean expression to boolean.
instance Evaluate Bexp Bool where
    evaluate bexp state = case bexp of
        Lit b -> b
        Or b1 b2 -> evaluate b1 state || evaluate b2 state
        And b1 b2 -> evaluate b1 state && evaluate b2 state
        Not b -> not (evaluate b state)
        Rel rop a1 a2 ->
            let
                v1 = evaluate a1 state
                v2 = evaluate a2 state
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
    evaluate stm state = case stm of
        Skip -> 0
        VarDef {} -> 1
        Seq s1 s2 -> evaluate s1 state + evaluate s2 state
        IfElse b s1 s2 ->
            if evaluate b state
                then evaluate s1 state
                else evaluate s2 state
        While b s ->
            if evaluate b state
                then evaluate s state + evaluate stm state
                else 0
        Print _ -> 0
        Read _ -> 1
        Local _ _ s -> evaluate s state + 1
        NonDet s1 s2 -> max (evaluate s1 state) (evaluate s2 state)
        Par s1 s2 -> evaluate s1 state + evaluate s2 state
        ProcDef _ -> 0
        ProcInvoc name (args, _) -> case getProc state name of
            Just p -> evaluate (procbody p) state + (toInteger . length) args
            Nothing -> 0
        Restore _ -> error $ "illegal statement for evaluate: " ++ show stm
        Return _ _ -> error $ "illegal statement for evaluate: " ++ show stm
        Break -> 0
        Revert s _ -> evaluate s state
        Match a ms d -> do
            let v = evaluate a state
            case lookup v ms of
                Just s -> evaluate s state
                Nothing -> evaluate d state
        Havoc _ -> 1
        Assert _ -> 0
        FlipFlop i s1 s2 ->
            if getFlip state i
                then evaluate s1 state
                else evaluate s2 state
        Raise _ -> 0
        TryCatch s1 _ s2 -> max (evaluate s1 state) (evaluate s2 state + 1)
        Swap _ _ -> 2
        Timeout s a -> min (evaluate s state) (evaluate a state)
        Alternate s1 s2 -> evaluate s1 state + evaluate s2 state
