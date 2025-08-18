{- FOURMOLU_DISABLE -}

{- |
Module      : IMP.Syntax
Description : Defines the syntax of the IMP language, including expressions, statements, and procedures.
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

__TODO__
-}
module IMP2.Syntax (
    Aexp (..),
    Aop (..),
    Bexp (..),
    Rop (..),
    Stm (..),
    Dop (..),
    Proc (..),
    Construct (..),
    Variables,
    variables,
)
where

import Data.List (nub)

-- | __TODO__
data Aexp
    = Bin Aop Aexp Aexp
    | Var String
    | Val Integer
    | Time Stm
    deriving (Eq, Show)

-- | __TODO__
instance Num Aexp where
    (+) = Bin Add
    (-) = Bin Sub
    (*) = Bin Mul
    fromInteger = Val
    abs (Val v) = Val $ abs v
    abs _ = error "abs not supported for abstract syntax"
    signum (Val v) = Val $ signum v
    signum _ = error "signum not supported for abstract syntax"

-- | __TODO__
instance Ord Aexp where
    Val v1 <= Val v2 = v1 <= v2
    _ <= _ = error "order not supported for abstract syntax"

-- | __TODO__
instance Enum Aexp where
    toEnum = Val . toEnum
    fromEnum (Val v) = fromEnum v
    fromEnum _ = error "fromEnum not supported for abstract syntax"

-- | __TODO__
instance Real Aexp where
    toRational (Val v) = toRational v
    toRational _ = error "toRational not supported for abstract syntax"

-- | __TODO__
instance Integral Aexp where
    div = Bin Div
    mod = Bin Mod
    quotRem a1 a2 = (div a1 a2, mod a1 a2)
    toInteger (Val v) = v
    toInteger _ = error "toInteger not supported for abstract syntax"

-- | __TODO__
data Aop
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Eq, Show)

-- | __TODO__
data Bexp
    = Or Bexp Bexp
    | And Bexp Bexp
    | Not Bexp
    | Rel Rop Aexp Aexp
    | Lit Bool
    deriving (Eq, Show)

-- | __TODO__
data Rop
    = Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq
    deriving (Eq, Show)

-- | __TODO__
data Dop
    = Def
    | Inc
    | Dec
    | Prod
    | Quot
    | Rem
    deriving (Eq, Show)

-- | __TODO__
data Stm
    = Skip
    | VarDef String Dop Aexp
    | Seq Stm Stm
    | IfElse Bexp Stm Stm
    | While Bexp Stm
    | Print Aexp
    | Read String
    | Local String Aexp Stm
    | Par Stm Stm
    | NonDet Stm Stm
    | ProcDef Proc
    | ProcInvoc String ([Aexp], [String])
    | Restore
        ([(String, Integer)], [Proc], Bool)
    | Return [String] [String]
    | Break
    | Revert Stm Bexp
    | Match Aexp [(Integer, Stm)] Stm
    | Havoc String
    | Assert Bexp
    | FlipFlop Integer Stm Stm
    | Raise Aexp
    | TryCatch Stm String Stm
    | Swap String String
    | Timeout Stm Aexp
    | Alternate Stm Stm
    deriving (Eq, Show)

-- | __TODO__
data Proc = Procedure
    { procname :: String
    , procsign :: ([String], [String])
    , procbody :: Stm
    }
    deriving (Eq)

-- | __TODO__
instance Show Proc where
    show p = unwords ["Procedure", show $ procname p, show $ procsign p, show $ procbody p]

-- | __TODO__
data Construct
    = Statement Stm
    | Arithmetic Aexp
    | Boolean Bexp
    | Whitespace
    deriving (Eq, Show)

-- | __TODO__
class Variables a where
    variables :: a -> [String]

-- | __TODO__
instance Variables Bexp where
    variables bexp = nub $ case bexp of
        Or b1 b2 -> variables b1 ++ variables b2
        And b1 b2 -> variables b1 ++ variables b2
        Not b -> variables b
        Rel _ a1 a2 -> variables a1 ++ variables a2
        Lit _ -> []

-- | __TODO__
instance Variables Aexp where
    variables aexp = nub $ case aexp of
        Bin _ a1 a2 -> variables a1 ++ variables a2
        Var x -> [x]
        Val _ -> []
        Time s -> variables s

-- | __TODO__
instance Variables Stm where
    variables stm = nub $ case stm of
        Skip -> []
        VarDef x _ a -> x : variables a
        Seq s1 s2 -> variables s1 ++ variables s2
        IfElse b s1 s2 -> variables b ++ variables s1 ++ variables s2
        While b s -> variables b ++ variables s
        Print a -> variables a
        Read x -> [x]
        Local x a s -> x : variables a ++ variables s
        Par s1 s2 -> variables s1 ++ variables s2
        NonDet s1 s2 -> variables s1 ++ variables s2
        ProcDef (Procedure p (ps, rs) s) -> p : ps ++ rs ++ variables s
        ProcInvoc p (as, rs) -> p : rs ++ concatMap variables as
        Restore _ -> error $ "illegal statement for free variables: " ++ show stm
        Return _ _ -> error $ "illegal statement for free variables: " ++ show stm
        Break -> []
        Revert s b -> variables s ++ variables b
        Match a ms d -> variables d ++ variables a ++ concatMap (variables . snd) ms
        Havoc x -> [x]
        Assert b -> variables b
        FlipFlop _ s1 s2 -> variables s1 ++ variables s2
        Raise a -> variables a
        TryCatch s1 x s2 -> x : variables s1 ++ variables s2
        Swap x y -> [x, y]
        Timeout s a -> variables s ++ variables a
        Alternate s1 s2 -> variables s1 ++ variables s2
