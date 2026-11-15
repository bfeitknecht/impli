{- |
Module      : IMP.Check
Description : Static checks for the IMP language
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Static well-formedness checks performed before execution.
Currently, every @break@ must be lexically enclosed by a loop, where
procedure bodies and @time@ statements start a fresh context.
-}
module IMP.Check (
    check,
) where

import Control.Monad (unless)

import IMP.Exception
import IMP.Syntax

-- | Check statement for well-formedness.
check :: Stm -> Either Exception ()
check = stm False

-- | Check statement, whether inside loop.
stm :: Bool -> Stm -> Either Exception ()
stm looping s = case s of
    Skip -> ok
    VarDef _ _ a -> aexp a
    Seq s1 s2 -> stm looping s1 >> stm looping s2
    IfElse b s1 s2 -> bexp b >> stm looping s1 >> stm looping s2
    While b s' -> bexp b >> stm True s'
    Print a -> aexp a
    Read _ -> ok
    Local _ a s' -> aexp a >> stm looping s'
    Par s1 s2 -> stm looping s1 >> stm looping s2
    NonDet s1 s2 -> stm looping s1 >> stm looping s2
    ProcDef _ _ _ body -> stm False body
    ProcInvoc _ as _ -> mapM_ aexp as
    Break -> unless looping . Left $ Error "break outside loop"
    Revert s' b -> stm looping s' >> bexp b
    Match a ms d -> aexp a >> mapM_ (stm looping . snd) ms >> stm looping d
    Havoc _ -> ok
    Assert b -> bexp b
    FlipFlop _ s1 s2 -> stm looping s1 >> stm looping s2
    Raise a -> aexp a
    TryCatch s1 _ s2 -> stm looping s1 >> stm looping s2
    Swap _ _ -> ok
    Timeout s' a -> stm looping s' >> aexp a
    Alternate s1 s2 -> stm looping s1 >> stm looping s2
    where
        ok = Right ()

-- | Check arithmetic expression.
aexp :: Aexp -> Either Exception ()
aexp a = case a of
    Bin _ a1 a2 -> aexp a1 >> aexp a2
    Time s -> stm False s
    _ -> Right ()

-- | Check boolean expression.
bexp :: Bexp -> Either Exception ()
bexp b = case b of
    Or b1 b2 -> bexp b1 >> bexp b2
    And b1 b2 -> bexp b1 >> bexp b2
    Not b' -> bexp b'
    Rel _ a1 a2 -> aexp a1 >> aexp a2
    Lit _ -> Right ()
