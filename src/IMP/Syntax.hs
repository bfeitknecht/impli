{- FOURMOLU_DISABLE -}

{- |
Module      : IMP.Syntax
Description : Syntax definition of the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.Syntax (
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

-- | TODO
data Aexp
    = Bin Aop Aexp Aexp -- ^ TODO
    | Var String -- ^ TODO
    | Val Integer -- ^ TODO
    | Time Stm -- ^ TODO
    deriving (Eq, Show)

-- | TODO
instance Num Aexp where
    (+) = Bin Add
    (-) = Bin Sub
    (*) = Bin Mul
    fromInteger = Val
    abs (Val v) = Val $ abs v
    abs _ = error "abs not supported for abstract syntax"
    signum (Val v) = Val $ signum v
    signum _ = error "signum not supported for abstract syntax"

-- | TODO
instance Ord Aexp where
    Val v1 <= Val v2 = v1 <= v2
    _ <= _ = error "order not supported for abstract syntax"

-- | TODO
instance Enum Aexp where
    toEnum = Val . toEnum
    fromEnum (Val v) = fromEnum v
    fromEnum _ = error "fromEnum not supported for abstract syntax"

-- | TODO
instance Real Aexp where
    toRational (Val v) = toRational v
    toRational _ = error "toRational not supported for abstract syntax"

-- | TODO
instance Integral Aexp where
    div = Bin Div
    mod = Bin Mod
    quotRem a1 a2 = (div a1 a2, mod a1 a2)
    toInteger (Val v) = v
    toInteger _ = error "toInteger not supported for abstract syntax"

-- | TODO
data Aop
    = Add -- ^ TODO
    | Sub -- ^ TODO
    | Mul -- ^ TODO
    | Div -- ^ TODO
    | Mod -- ^ TODO
    deriving (Eq, Show)

-- | TODO
data Bexp
    = Or Bexp Bexp -- ^ TODO
    | And Bexp Bexp -- ^ TODO
    | Not Bexp -- ^ TODO
    | Rel Rop Aexp Aexp -- ^ TODO
    | Lit Bool -- ^ TODO
    deriving (Eq, Show)

-- | TODO
data Rop
    = Eq -- ^ TODO
    | Neq -- ^ TODO
    | Lt -- ^ TODO
    | Leq -- ^ TODO
    | Gt -- ^ TODO
    | Geq -- ^ TODO
    deriving (Eq, Show)

-- | TODO
data Dop
    = Def -- ^ TODO
    | Inc -- ^ TODO
    | Dec -- ^ TODO
    | Prod -- ^ TODO
    | Quot -- ^ TODO
    | Rem -- ^ TODO
    deriving (Eq, Show)

-- | TODO
data Stm
    = Skip -- ^ TODO
    | VarDef String Dop Aexp -- ^ TODO
    | Seq Stm Stm -- ^ TODO
    | IfElse Bexp Stm Stm -- ^ TODO
    | While Bexp Stm -- ^ TODO
    | Print Aexp -- ^ TODO
    | Read String -- ^ TODO
    | Local String Aexp Stm -- ^ TODO
    | Par Stm Stm -- ^ TODO
    | NonDet Stm Stm -- ^ TODO
    | ProcDef Proc -- ^ TODO
    | ProcInvoc String ([Aexp], [String]) -- ^ TODO
    | Restore ([(String, Integer)], [Proc], Bool) -- ^ TODO
    | Return [String] [String] -- ^ TODO
    | Break -- ^ TODO
    | Revert Stm Bexp -- ^ TODO
    | Match Aexp [(Integer, Stm)] Stm -- ^ TODO
    | Havoc String -- ^ TODO
    | Assert Bexp -- ^ TODO
    | FlipFlop Integer Stm Stm -- ^ TODO
    | Raise Aexp -- ^ TODO
    | TryCatch Stm String Stm -- ^ TODO
    | Swap String String -- ^ TODO
    | Timeout Stm Aexp -- ^ TODO
    | Alternate Stm Stm -- ^ TODO
    deriving (Eq, Show)

-- | TODO
instance Semigroup Stm where
    (<>) = Seq

-- | TODO
instance Monoid Stm where
    mempty = Skip

-- | TODO
data Proc = Procedure
    { procname :: String -- ^ TODO
        , procsign :: ([String], [String]) -- ^ TODO
        , procbody :: Stm -- ^ TODO
    }
    deriving (Eq)

-- | TODO
instance Show Proc where
    show p = unwords ["Procedure", show $ procname p, show $ procsign p, show $ procbody p]

-- | TODO
data Construct
    = Statement Stm -- ^ TODO
    | Arithmetic Aexp -- ^ TODO
    | Boolean Bexp -- ^ TODO
    | Whitespace -- ^ TODO
    deriving (Eq, Show)

-- | TODO
class Variables a where
    variables :: a -> [String]

-- | TODO
instance Variables Bexp where
    variables bexp = nub $ case bexp of
        Or b1 b2 -> variables b1 ++ variables b2
        And b1 b2 -> variables b1 ++ variables b2
        Not b -> variables b
        Rel _ a1 a2 -> variables a1 ++ variables a2
        Lit _ -> []

-- | TODO
instance Variables Aexp where
    variables aexp = nub $ case aexp of
        Bin _ a1 a2 -> variables a1 ++ variables a2
        Var x -> [x]
        Val _ -> []
        Time s -> variables s

-- | TODO
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
        Restore _ -> error $ "illegal statement for variables: " ++ show stm
        Return _ _ -> error $ "illegal statement for variables: " ++ show stm
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
