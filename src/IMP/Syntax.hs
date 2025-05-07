module IMP.Syntax where

type Ident = String -- identifier
type Val = Integer -- unbounded numerals

-- arithmetic expression
data Aexp
    = Bin Aop Aexp Aexp
    | Variable Ident
    | Numeral Val
    | Time Stm
    deriving (Eq, Show)

-- arithmetic operation
data Aop = Add | Sub | Mul deriving (Eq, Show)

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

-- statement
data Stm
    = Skip -- skip
    | VarDef Ident Aexp -- x := e
    | Seq Stm Stm -- s1; s2
    | If Bexp Stm Stm -- if b then s1 else s2 end
    | While Bexp Stm -- while b do s end
    | Print Aexp -- print e
    | Read Ident -- read x
    | Local Ident Aexp Stm -- var x := e in s end
    | Par Stm Stm -- s1 par s2
    | NonDet Stm Stm -- s1 || s2
    | ProcDef Proc -- procedure p(params; rets) begin s end
    | ProcInvoc Ident ([Aexp], [Ident]) -- p(args; rets)
    | Break -- break
    | Revert Stm Bexp -- revert s if b
    deriving (Eq, Show)

inc :: Ident -> Stm
inc x = VarDef x (Bin Add (Variable x) (Numeral 1))

dec :: Ident -> Stm
dec x = VarDef x (Bin Sub (Variable x) (Numeral 1))

data Proc = Proc
    { procname :: Ident
    , procsign :: ([Ident], [Ident])
    , procbody :: Stm
    }
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
