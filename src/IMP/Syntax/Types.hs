module IMP.Syntax.Types where

-- arithmetic expression
data Aexp
    = Bin Aop Aexp Aexp
    | Variable String
    | Numeral Integer
    | Time Stm
    deriving (Eq, Show)

-- arithmetic operation
data Aop
    = Add
    | Sub
    | Mul
    deriving (Eq, Show)

-- boolean expression
data Bexp
    = Or Bexp Bexp -- b1 or b2
    | And Bexp Bexp -- b1 and b2
    | Not Bexp -- not b
    | Rel Rop Aexp Aexp -- e1 r e2, where r in {=, #, <, <=, >, >=}
    | Boolean Bool
    deriving (Eq, Show)

-- relation operation
data Rop
    = Eq -- =
    | Neq -- #
    | Lt -- <
    | Leq -- <=
    | Gt -- >
    | Geq -- >=
    deriving (Eq, Show)

data Dop
    = Id -- :=
    | Inc -- +=
    | Dec -- -=
    | Prod
    -- \*=
    deriving (Eq, Show)

-- statement
data Stm
    = Skip -- skip
    | VarDef String Dop Aexp -- x f e, where f in {:=, +=, -+, *=}
    | Seq Stm Stm -- s1; s2
    | If Bexp Stm Stm -- if b then s1 else s2 end
    | While Bexp Stm -- while b do s end
    | Print Aexp -- print e
    | Read String -- read x
    | Local String Aexp Stm -- var x := e in s end
    | Par Stm Stm -- s1 par s2
    | NonDet Stm Stm -- s1 || s2
    | ProcDef Proc -- procedure p(params; rets) begin s end
    | ProcInvoc String ([Aexp], [String]) -- p(args; rets)
    | Break -- break
    | Revert Stm Bexp -- revert s if b
    | Match Aexp [(Integer, Stm)] Stm -- match e on {v: s,} default: s
    | Havoc String -- havoc x
    | Assert Bexp -- assert b
    | Flip Integer Stm Stm -- flip(i) s1 flop s2 end
    | Raise Aexp -- raise e
    | Try Stm String Stm -- try s1 catch x with s2 end
    deriving (Eq, Show)

data Proc = Proc String ([String], [String]) Stm deriving (Eq, Show)

procname :: Proc -> String
procname (Proc p _ _) = p

data Construct
    = Statement Stm
    | Arithm Aexp
    | Bool Bexp
    | Whitespace
    deriving (Eq)

instance Show Construct where
    show construct = case construct of
        Statement s -> show s
        Arithm e -> show e
        Bool b -> show b
        Whitespace -> "/**/"
