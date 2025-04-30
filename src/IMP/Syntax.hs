module IMP.Syntax where

type Var = String -- identifier
type Val = Integer -- unbounded numerals

-- arithmetic expression
data Aexp
    = Bin Aop Aexp Aexp
    | Variable Var
    | Numeral Val
    deriving (Eq, Show)

-- arithmetic operation
data Aop = Add | Sub | Mul deriving (Eq, Show)

-- boolean expression
data Bexp
    = Or Bexp Bexp -- (b1 or b2)
    | And Bexp Bexp -- (b1 and b2)
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

-- statement
data Stm
    = Skip -- skip
    | Print Aexp -- print e
    | VarDef Var Aexp -- x := e
    | Seq Stm Stm -- (s1; s2)
    | If Bexp Stm Stm -- if b then s1 else s2 end
    | While Bexp Stm -- while b do s end
    | Local Var Aexp Stm -- var x := e in s end
    | Par Stm Stm -- s1 par s2
    | NonDet Stm Stm -- s1 || s2
    | ProcDef Var ([Var], [Var]) Stm -- procedure p(params; rets) begin s end
    | ProcInvoc Var ([Aexp], [Var]) -- p(args; rets)
    deriving (Eq, Show)

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
