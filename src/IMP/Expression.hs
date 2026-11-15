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
Evaluation is pure but may fail, since @time s@ executes @s@ to count the
steps it takes, see 'IMP.Semantics.Structural.measure'.
-}
module IMP.Expression (
    Evaluate,
    evaluate,
) where

import IMP.Exception
import {-# SOURCE #-} IMP.Semantics.Structural (measure)
import IMP.State
import IMP.Syntax

-- | Typeclass to evaluate functional dependency.
class Evaluate a b | a -> b where
    -- | Evaluate type in the context of some state to unique type, or fail.
    evaluate :: a -> State -> Either Exception b

-- | Evaluate arithmetic expression to integer value.
instance Evaluate Aexp Integer where
    evaluate aexp state = case aexp of
        Val n -> Right n
        Var x -> Right $ getVar state x
        Bin aop a1 a2 -> do
            v1 <- evaluate a1 state
            v2 <- evaluate a2 state
            Right $ case aop of
                Add -> v1 + v2
                Sub -> v1 - v2
                Mul -> v1 * v2
                Div -> v1 // v2
                Mod -> v1 %% v2
        Time s -> measure s state

-- | Evaluate boolean expression to truth value.
instance Evaluate Bexp Bool where
    evaluate bexp state = case bexp of
        Lit b -> Right b
        Or b1 b2 -> evaluate b1 state >>= \v -> if v then Right True else evaluate b2 state
        And b1 b2 -> evaluate b1 state >>= \v -> if v then evaluate b2 state else Right False
        Not b -> not <$> evaluate b state
        Rel rop a1 a2 -> do
            v1 <- evaluate a1 state
            v2 <- evaluate a2 state
            Right $ case rop of
                Eq -> v1 == v2
                Neq -> v1 /= v2
                Lt -> v1 < v2
                Leq -> v1 <= v2
                Gt -> v1 > v2
                Geq -> v1 >= v2
