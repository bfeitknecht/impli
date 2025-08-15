{- FOURMOLU_DISABLE -}

{- |
Module      : IMP.Syntax
Description : Defines the syntax of the IMP language, including expressions, statements, and procedures.
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides the syntax definitions for the IMP language. It includes data types for
arithmetic expressions, boolean expressions, statements, and procedures. These types form
the abstract syntax tree (AST) that is constructed by "IMP.Parser", manipulated by
"IMP.Semantic.Statement" and "IMP.Semantic.Expression", and displayed using "IMP.Pretty".
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
)
where

import Data.List

-- | Arithmetic expressions. Parsed by "IMP.Parser" and evaluated by "IMP.Semantic.Expression".
data Aexp
    = Bin Aop Aexp Aexp     -- ^ Binary operation, @e1 aop e2@ (addition, subtraction, etc.).
    | Var String            -- ^ Variable reference, @x@ (resolved using "IMP.Semantic.State").
    | Val Integer           -- ^ Integer value, @n@ (constant value).
    | Time Stm              -- ^ Number of variable definitions, @time s@ (execution complexity metric).
    deriving (Eq, Show)

-- | Partial implementation of 'Num'.
instance Num Aexp where
    (+) = Bin Add
    (-) = Bin Sub
    (*) = Bin Mul
    fromInteger = Val
    abs (Val v) = Val $ abs v
    abs _ = error "abs not supported for abstract syntax"
    signum (Val v) = Val $ signum v
    signum _ = error "signum not supported for abstract syntax"

-- | Partial implementation of 'Ord'.
instance Ord Aexp where
    Val v1 <= Val v2 = v1 <= v2
    _ <= _ = error "order not supported for abstract syntax"

-- | Partial implementation of 'Enum'.
instance Enum Aexp where
    toEnum = Val . toEnum
    fromEnum (Val v) = fromEnum v
    fromEnum _ = error "fromEnum not supported for abstract syntax"

-- | Partial implementation of 'Real'.
instance Real Aexp where
    toRational (Val v) = toRational v
    toRational _ = error "toRational not supported for abstract syntax"

-- | Partial implementation of 'Integral'.
instance Integral Aexp where
    div = Bin Div
    mod = Bin Mod
    quotRem e1 e2 = (div e1 e2, mod e1 e2)
    toInteger (Val v) = v
    toInteger _ = error "toInteger not supported for abstract syntax"

-- | Arithmetic operators.
-- Used in 'Bin' constructor of 'Aexp' for binary operations.
data Aop
    = Add -- ^ Addition, @+@ (implemented with 'Prelude.+').
    | Sub -- ^ Subtraction, @-@ (implemented with 'Prelude.-').
    | Mul -- ^ Multiplication, @*@ (implemented with '*').
    | Div -- ^ Division, @/@ (implemented with 'IMP.Util.//' for safe division).
    | Mod -- ^ Modulo, @%@ (implemented with 'IMP.Util.%%' for safe modulo).
    deriving (Eq, Show)

-- | Boolean expressions. Parsed by "IMP.Parser" and evaluated by "IMP.Semantic.Expression".
data Bexp
    = Or Bexp Bexp          -- ^ Logical OR, @b1 or b2@ (implemented with 'Prelude.||', short-circuits).
    | And Bexp Bexp         -- ^ Logical AND, @b1 and b2@ (implemented with 'Prelude.&&', short-circuits).
    | Not Bexp              -- ^ Logical NOT, @not b@ (boolean negation).
    | Rel Rop Aexp Aexp     -- ^ Relation, @e1 rop e2@ (comparison between arithmetic expressions, 'Aexp').
    | Lit Bool              -- ^ Boolean literal, @true@, @false@ (constant value).
    deriving (Eq, Show)

-- | Relational operators.
-- Used in 'Rel' constructor of 'Bexp' for comparing arithmetic expressions.
data Rop
    = Eq   -- ^ Equality, @=@ (tests if LHS and RHS evaluate to same value).
    | Neq  -- ^ Inequality, @#@ (tests if LHS and RHS evaluate to different values).
    | Lt   -- ^ Less than, @<@ (tests if LHS evaluates to less than right).
    | Leq  -- ^ Less than or equal, @<=@ (tests if LHS evaluates to less than or equal to right).
    | Gt   -- ^ Greater than, @>@ (tests if LHS expression evalautes to greater than right).
    | Geq  -- ^ Greater than or equal, @>=@ (tests if LHS evaluates to greater than or equal to right).
    deriving (Eq, Show)

-- | Variable definition operators.
-- Used in 'VarDef' constructor of 'Stm' for variable definitions.
data Dop
    = Def  -- ^ Definition, @,=@ (direct assignment of value).
    | Inc  -- ^ Increment, @+=@ (add to current value).
    | Dec  -- ^ Decrement, @-=@ (subtract from current value).
    | Prod -- ^ Product, @*=@ (multiply current value).
    | Quot -- ^ Quotient, @/=@ (divide current value, using safe division).
    | Rem  -- ^ Remainder, @%=@ (remainder of division, using safe modulo).
    deriving (Eq, Show)

-- | Statements. The core construct of IMP programs, parsed by "IMP.Parser" and executed by "IMP.Semantic.Statement".
data Stm
    = Skip                                  -- ^ No-op statement, @skip@ (does nothing).
    | VarDef String Dop Aexp                -- ^ Var definition, @x dop e@ (assigns value to variable).
    | Seq Stm Stm                           -- ^ Sequence, @s1; s2@ (executes statements in order).
    | If Bexp Stm Stm                       -- ^ Conditional, @if b then s1 else s2 end@ (branching execution).
    | While Bexp Stm                        -- ^ While loop, @while b do s end@ (conditional iteration).
    | Print Aexp                            -- ^ Print expression, @print e@ (outputs value to user).
    | Read String                           -- ^ Read input, @read x@ (gets input from user).
    | Local String Aexp Stm                 -- ^ Local variable, @var x := e in s end@ (scoped variable).
    | Par Stm Stm                           -- ^ Parallel composition, @s1 par s2@ (simulates concurrency, only in small-step).
    | NonDet Stm Stm                        -- ^ Non-deterministic execution, @s1 [] s2@ (random choice).
    | ProcDef Proc                          -- ^ Procedure definition, @procedure p(params; rets) begin s end@.
    | ProcInvoc String ([Aexp], [String])   -- ^ Procedure invocation, @p(args; rets)@ (procedure call).
    | Restore                               -- ^ Restore variables, procedures and break flag (internal only).
        ([(String, Integer)], [Proc], Bool)
    | Return [String] [String]              -- ^ Return variables to callside (internal only).
    | Break                                 -- ^ Break statement, @break@ (exits nearest enclosing loop).
    | Revert Stm Bexp                       -- ^ Transactional statement, @revert s if b@ (conditional state rollback).
    | Match Aexp [(Integer, Stm)] Stm       -- ^ Pattern match, @match e on {v: s,} default: s@ (switch-case construct).
    | Havoc String                          -- ^ Random variable definition, @havoc x@ (assigns random value).
    | Assert Bexp                           -- ^ Assertion, @assert b@ (verifies program property).
    | Flip Integer Stm Stm                  -- ^ Alternating branches, @flip(i) s1 flop s2 end@ (toggles execution path).
    | Raise Aexp                            -- ^ Raise exception, @raise e@ (throws exception with value).
    | Try Stm String Stm                    -- ^ Exception handling, @try s1 catch x with s2 end@ (catches exceptions).
    | Swap String String                    -- ^ Swap variables, @swap x y@ (exchanges variable values).
    | Timeout Stm Aexp                      -- ^ Execution with timeout, @timeout s after e end@ (bounded execution).
    | Alternate Stm Stm                     -- ^ Alternating execution, @s1 alternate s2@ (specialized concurrency).
    deriving (Eq, Show)

-- | Procedure encapsulation. Represents invokeable procedure.
data Proc = Procedure
    { procname :: String                -- ^ Procedure name used for invocation.
    , procsign :: ([String], [String])  -- ^ Procedure signature with variable names of parameters and returns.
    , procbody :: Stm                   -- ^ Procedure body, statement to execute.
    }
    deriving (Eq)

instance Show Proc where
    show p = unwords ["Procedure", show $ procname p, show $ procsign p, show $ procbody p]

-- | IMP constructs. Top-level elements that can be parsed by "IMP.Parser" and processed by "IMP.REPL".
data Construct
    = Statement Stm         -- ^ Statement to be executed
    | Arithmetic Aexp       -- ^ Arithmetic expression to be evaluated
    | Boolean Bexp          -- ^ Boolean expression to be evaluated
    | Whitespace            -- ^ Whitespace or comment (ignored during execution)
    deriving (Eq, Show)

class Variables a where
    variables :: a -> [String]

instance Variables Bexp where
    variables bexp = case bexp of
        Or b1 b2 -> union (variables b1) (variables b2)
        And b1 b2 -> union (variables b1) (variables b2)
        Not b -> variables b
        Rel _ e1 e2 -> union (variables e1) (variables e2)
        Lit _ -> []

instance Variables Aexp where
    variables aexp = case aexp of
        Bin _ e1 e2 -> union (variables e1) (variables e2)
        Var x -> [x]
        Val _ -> []
        Time s -> variables s

instance Variables Stm where
    variables stm = case stm of
        Skip -> []
