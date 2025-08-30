{- FOURMOLU_DISABLE -}

{- |
Module      : IMP.Syntax
Description : Syntax definition for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Abstract syntax definition for the IMP language.
Provides arithmetic expression, boolean expression, statement, and procedure definition.
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

-- | Arithmetic expression in the IMP language.
data Aexp
    = Bin Aop Aexp Aexp -- ^ Binary operation.
    | Var String -- ^ Variable.
    | Val Integer -- ^ Integer value.
    | Time Stm -- ^ Atomic execution metric.
    deriving (Eq, Show)

-- | Instance of 'Num' for 'Aexp' to allow numerical operations.
instance Num Aexp where
    (+) = Bin Add
    (-) = Bin Sub
    (*) = Bin Mul
    fromInteger = Val
    abs (Val v) = Val $ abs v
    abs _ = error "abs not supported for abstract syntax"
    signum (Val v) = Val $ signum v
    signum _ = error "signum not supported for abstract syntax"

-- | Instance of 'Ord' for 'Aexp' to allow ordering.
instance Ord Aexp where
    Val v1 <= Val v2 = v1 <= v2
    _ <= _ = error "order not supported for abstract syntax"

-- | Instance of 'Enum' for 'Aexp' to allow conversion with 'Int'.
instance Enum Aexp where
    toEnum = Val . toEnum
    fromEnum (Val v) = fromEnum v
    fromEnum _ = error "fromEnum not supported for abstract syntax"

-- | Instance of 'Real' for 'Aexp' to allow conversion with 'Rational'.
instance Real Aexp where
    toRational (Val v) = toRational v
    toRational _ = error "toRational not supported for abstract syntax"

-- | Instance of 'Integral' for 'Aexp' to allow integer operations.
instance Integral Aexp where
    div = Bin Div
    mod = Bin Mod
    quotRem a1 a2 = (div a1 a2, mod a1 a2)
    toInteger (Val v) = v
    toInteger _ = error "toInteger not supported for abstract syntax"

-- | Arithmetic operator for binary operations.
data Aop
    = Add -- ^ Addition.
    | Sub -- ^ Subtraction.
    | Mul -- ^ Multiplication.
    | Div -- ^ Division.
    | Mod -- ^ Modulus.
    deriving (Eq, Show)

-- | Boolean expression in the IMP language.
data Bexp
    = Or Bexp Bexp -- ^ Disjunction.
    | And Bexp Bexp -- ^ Conjunction.
    | Not Bexp -- ^ Negation.
    | Rel Rop Aexp Aexp -- ^ Relation.
    | Lit Bool -- ^ Literal.
    deriving (Eq, Show)

-- | Relational operator for relations.
data Rop
    = Eq -- ^ Equality.
    | Neq -- ^ Inequality.
    | Lt -- ^ Less.
    | Leq -- ^ Less or equal.
    | Gt -- ^ Greater.
    | Geq -- ^ Greater or equal.
    deriving (Eq, Show)

-- | Definition operators for variable definitions.
data Dop
    = Def -- ^ Definition.
    | Inc -- ^ Increment.
    | Dec -- ^ Decrement.
    | Prod -- ^ Product.
    | Quot -- ^ Quotient.
    | Rem -- ^ Remainder.
    deriving (Eq, Show)

-- | Statement in the IMP language.
data Stm
    = Skip -- ^ Do nothing.
    | VarDef String Dop Aexp -- ^ Variable definition.
    | Seq Stm Stm -- ^ Sequential composition.
    | IfElse Bexp Stm Stm -- ^ Conditional branching.
    | While Bexp Stm -- ^ While loop.
    | Print Aexp -- ^ Output evaluation of arithmetic expression.
    | Read String -- ^ Read input into variable.
    | Local String Aexp Stm -- ^ Local variable.
    | Par Stm Stm -- ^ Parallel execution.
    | NonDet Stm Stm -- ^ Non-deterministic choice.
    | ProcDef Proc -- ^ Procedure definition.
    | ProcInvoc String ([Aexp], [String]) -- ^ Procedure invocation.
    | Restore ([(String, Integer)], [Proc], Bool) -- ^ Restore state (internal use).
    | Return [String] [String] -- ^ Return values (internal use).
    | Break -- ^ Break loop.
    | Revert Stm Bexp -- ^ Transactional execution.
    | Match Aexp [(Integer, Stm)] Stm -- ^ Pattern match on integer values.
    | Havoc String -- ^ Random value variable definition.
    | Assert Bexp -- ^ Assert boolean condition.
    | FlipFlop Integer Stm Stm -- ^ Alternating execution.
    | Raise Aexp -- ^ Raise exception with value.
    | TryCatch Stm String Stm -- ^ Exception handling.
    | Swap String String -- ^ Swap values of two variables.
    | Timeout Stm Aexp -- ^ Execute with timeout.
    | Alternate Stm Stm -- ^ Interleaved execution.
    deriving (Eq, Show)

-- | Instance of 'Semigroup' for 'Stm' allowing sequential composition with 'Seq'.
instance Semigroup Stm where
    (<>) = Seq

-- | Instance of 'Monoid' for 'Stm', with identity element 'Skip'.
instance Monoid Stm where
    mempty = Skip

-- | Procedure definition in the IMP language.
data Proc = Procedure
    { procname :: String -- ^ Name of procedure.
    , procsign :: ([String], [String]) -- ^ Parameters and return variable.
    , procbody :: Stm -- ^ Body of procedure.
    }
    deriving (Eq)

-- | Instance of 'Show' for 'Proc'.
instance Show Proc where
    show p = unwords ["Procedure", show $ procname p, show $ procsign p, show $ procbody p]

-- | Construct in the IMP language.
data Construct
    = Statement Stm -- ^ Statement.
    | Arithmetic Aexp -- ^ Arithmetic expression.
    | Boolean Bexp -- ^ Boolean expression.
    | Whitespace -- ^ Whitespace, i.e. comment.
    deriving (Eq, Show)

-- | Typeclass for extracting free variables.
class Variables a where
    -- | Extract free variables.
    variables :: a -> [String]

-- | Extract free variables from boolean expression.
instance Variables Bexp where
    variables bexp = nub $ case bexp of
        Or b1 b2 -> variables b1 ++ variables b2
        And b1 b2 -> variables b1 ++ variables b2
        Not b -> variables b
        Rel _ a1 a2 -> variables a1 ++ variables a2
        Lit _ -> []

-- | Extract free variables from arithmetic expression.
instance Variables Aexp where
    variables aexp = nub $ case aexp of
        Bin _ a1 a2 -> variables a1 ++ variables a2
        Var x -> [x]
        Val _ -> []
        Time s -> variables s

-- | Extract free variables from statement.
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
