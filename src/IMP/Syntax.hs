module IMP.Syntax where

type Var = String -- identifier
type Val = Integer -- unbounded numerals

-- arithmetic expression
data Aexp
    = Bin Op Aexp Aexp
    | Variable Var
    | Numeral Val
    deriving (Show)

-- arithmetic operation
data Op = Add | Sub | Mul deriving (Show)

-- boolean expression
data Bexp
    = Or Bexp Bexp -- (b1 or b2)
    | And Bexp Bexp -- (b1 and b2)
    | Not Bexp -- not b
    | Rel Rop Aexp Aexp -- e1 ◊ e2, where ◊ in {=, #, <, <=, >, >=}
    | Boolean Bool
    deriving (Show)

-- relation operation
data Rop
    = Eq -- =
    | Neq -- #
    | Lt -- <
    | Leq -- <=
    | Gt -- >
    | Geq -- >=
    deriving (Show)

-- statement
data Stm
    = Skip -- skip
    | Print Aexp -- print e
    | Def Var Aexp -- x := e
    | Seq Stm Stm -- (s; s')
    | If Bexp Stm Stm -- if b then s else s' end
    | While Bexp Stm -- while b do s end
    | Local Var Aexp Stm -- var x := e in s end
    | Par Stm Stm -- s par s'
    | NonDet Stm Stm -- s || s'
    | ProcDef Var [Var] [Var] Stm -- procedure p(params; rets) begin s end
    | ProcInvoc Var [Aexp] [Var] -- p(args; rets)
    deriving (Show)

data Construct
    = Statement Stm
    | Arithm Aexp
    | Bool Bexp
    deriving (Show)
