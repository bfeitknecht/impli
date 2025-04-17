module IMP.Syntax where

type Var = String           -- identifier
type Val = Integer          -- unbounded numerals

-- arithmetic expression
data Aexp = Bin Op Aexp Aexp
    | Variable Var
    | Numeral Val

-- arithmethic operation
data Op = Add | Sub | Mul

-- boolean expression
data Bexp = Or Bexp Bexp    -- (b1 or b2)
    | And Bexp Bexp         -- (b1 and b2)
    | Not Bexp              -- not b
    | Rel Rop Aexp Aexp     -- e1 ◊ e2, where ◊ in {=, #, <, <=, >, >=}

-- relation operation
data Rop = Eq               -- =
    | Neq                   -- #
    | Lt                    -- <
    | Leq                   -- <=
    | Gt                    -- >
    | Geq                   -- >=

-- statement
data Stm = Skip             -- skip
    | Print Aexp            -- print e
    | Assign Var Aexp       -- x := e
    | Seq Stm Stm           -- (s; s')
    | If Bexp Stm Stm       -- if b then s else s' end
    | While Bexp Stm        -- while b do s end
