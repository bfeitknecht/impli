{- FOURMOLU_DISABLE -}
{- |
Module      : IMP.Syntax
Description : Defines the syntax of the IMP language, including expressions, statements, and procedures.
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides the syntax definitions for IMP. It includes data types for
arithmetic expressions, boolean expressions, statements, and procedures.
-}
module IMP.Syntax where

-- | Arithmetic expressions.
data Aexp
    = Bin Aop Aexp Aexp -- ^ Binary operation: @e1 aop e2@
    | Variable String   -- ^ Variable reference: @x@
    | Numeral Integer   -- ^ Integer literal: @n@
    | Time Stm          -- ^ Number of variable definitions: @time s@
    deriving (Eq, Show)

-- | Arithmetic operators.
data Aop
    = Add -- ^ Addition: @+@
    | Sub -- ^ Subtraction: @-@
    | Mul -- ^ Multiplication: @*@
    | Div -- ^ Division: @/@
    | Mod -- ^ Modulo: @%@
    deriving (Eq, Show)

-- | Boolean expressions.
data Bexp
    = Or Bexp Bexp         -- ^ Logical OR: @b1 or b2@
    | And Bexp Bexp        -- ^ Logical AND: @b1 and b2@
    | Not Bexp             -- ^ Logical NOT: @not b@
    | Rel Rop Aexp Aexp    -- ^ Relation: @e1 rop e2@
    | Boolean Bool         -- ^ Boolean literal: @true@ or @false@
    deriving (Eq, Show)

-- | Relational operators.
data Rop
    = Eq   -- ^ Equality: @=@
    | Neq  -- ^ Inequality: @#@
    | Lt   -- ^ Less than: @<@
    | Leq  -- ^ Less than or equal: @<=@
    | Gt   -- ^ Greater than: @>@
    | Geq  -- ^ Greater than or equal: @>=@
    deriving (Eq, Show)

-- | Variable definition operators.
data Dop
    = Def  -- ^ Definition: @:=@
    | Inc  -- ^ Increment: @+=@
    | Dec  -- ^ Decrement: @-=@
    | Prod -- ^ Product: @*=@
    | Quot -- ^ Quotient: @/=@
    | Rem  -- ^ Remainder: @%=@
    deriving (Eq, Show)

-- | Statements.
data Stm
    = Skip                                  -- ^ No-op statement: @skip@
    | VarDef String Dop Aexp                -- ^ Variable definition: @x dop e@
    | Seq Stm Stm                           -- ^ Sequence: @s1; s2@
    | If Bexp Stm Stm                       -- ^ Conditional: @if b then s1 else s2 end@
    | While Bexp Stm                        -- ^ While loop: @while b do s end@
    | Print Aexp                            -- ^ Print expression: @print e@
    | Read String                           -- ^ Read input: @read x@
    | Local String Aexp Stm                 -- ^ Local variable: @var x := e in s end@
    | Par Stm Stm                           -- ^ Parallel composition: @s1 par s2@
    | NonDet Stm Stm                        -- ^ Non-deterministic execution: @s1 [] s2@
    | ProcDef Proc                          -- ^ Procedure definition: @procedure p(params; rets) begin s end@
    | ProcInvoc String ([Aexp], [String])   -- ^ Procedure invocation: @p(args; rets)@
    | Restore                               -- ^ Restore variables, procedures and break flag.
        ([(String, Integer)], [Proc], Bool)
    | Return [String]                       -- ^ Return variables to callside.
    | Break                                 -- ^ Break statement: @break@
    | Revert Stm Bexp                       -- ^ Transactional statement: @revert s if b@
    | Match Aexp [(Integer, Stm)] Stm       -- ^ Pattern match: @match e on {v: s,} default: s@
    | Havoc String                          -- ^ Random variable definition: @havoc x@
    | Assert Bexp                           -- ^ Assertion: @assert b@
    | Flip Integer Stm Stm                  -- ^ Alternating branches: @flip(i) s1 flop s2 end@
    | Raise Aexp                            -- ^ Raise exception: @raise e@
    | Try Stm String Stm                    -- ^ Exception handling: @try s1 catch x with s2 end@
    | Swap String String                    -- ^ Swap variables: @swap x y@
    deriving (Eq, Show)

-- | Procedure encapsulation.
data Proc = Proc String ([String], [String]) Stm deriving (Eq, Show)

-- | Get the name of a procedure.
procname :: Proc -> String
procname (Proc p _ _) = p

-- | IMP constructs.
data Construct
    = Statement Stm   -- ^ Statement
    | Arithm Aexp     -- ^ Arithmetic expression
    | Bool Bexp       -- ^ Boolean expression
    | Whitespace      -- ^ Whitespace or comment
    deriving (Eq)

instance Show Construct where
    show construct = case construct of
        Statement s -> show s
        Arithm e -> show e
        Bool b -> show b
        Whitespace -> "/**/"
