module IMP.Syntax where

type Var = String -- identifier
type Val = Integer -- unbounded numerals

-- arithmetic expression
data Aexp
    = Bin Op Aexp Aexp
    | Variable Var
    | Numeral Val
    deriving (Show)

-- arithmethic operation
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
    | Assign Var Aexp -- x := e
    | Seq Stm Stm -- (s; s')
    | If Bexp Stm Stm -- if b then s else s' end
    | While Bexp Stm -- while b do s end
    | Local Var Aexp Stm
    | Par Stm Stm
    | NonDet Stm Stm
    | ProcDef Var [Var] [Var] Stm
    | ProcInvoc Var [Aexp] [Var]
    deriving (Show)
