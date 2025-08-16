module IMP2.Semantic.State where

import qualified Control.Monad.Trans.Except as Except
import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Util
import IMP2.Exception
import IMP2.Syntax

type IMP = Except.ExceptT Exception IO

-- | Map from string identifiers to integer values.
type Vars = Map.Map String Integer

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, [Proc], Bool)

-- | Interpreter configuration as pair of state stack and remaining statement execution.
type Conf = ([State], Maybe Stm)

initial :: State
initial = (Map.empty, [], False)

getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | Set value of variable in state. The placeholder @_@ is ignored.
-- Used by "IMP.Semantic.Statement" for variable definitions.
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar _ "" _ = error "variable name can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | Set multiple variables in state.
-- Used by "IMP.Semantic.Statement" for multiple definitions like procedure returns.
setVars :: State -> [(String, Integer)] -> State
setVars = foldl (uncurry . setVar)

-- | Get procedure by name from state.
-- Used by "IMP.Semantic.Statement" when invoking procedures.
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

-- | Set procedure in state.
-- Used by "IMP.Semantic.Statement" when defining procedures.
setProc :: State -> Proc -> State
setProc (vars, procs, flag) proc = (vars, proc : procs, flag)

-- | Set break flag in state.
-- Used by "IMP.Semantic.Statement" when executing a @break@ statement.
setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

-- | Reset break flag in state.
-- Used by "IMP.Semantic.Statement" after processing a @break@ statement.
resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

-- | Generate variable name for flip index.
-- Used for implementing the @flip@/@flop@ construct in "IMP.Semantic.Statement".
flipvar :: Integer -> String
flipvar i = "_flip" ++ show i

-- | Get value of flip variable (flip if zero, otherwise flop).
-- Used by "IMP.Semantic.Statement" and "IMP.Semantic.Expression" for the @flip@ construct.
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | Set flip variable to flip.
-- Used by "IMP.Semantic.Statement" when toggling @flop@ to @flip@.
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | Set flip variable to flop.
-- Used by "IMP.Semantic.Statement" when toggling @flip@ to @flop@.
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | Safe integer division: returns zero if divisor is zero.
-- Used by "IMP.Semantic.Expression" to implement division that doesn't raise exceptions.A
(//) :: Integer -> Integer -> Integer
(//) v1 v2 = if v2 == 0 then 0 else div v1 v2

-- | Safe integer modulo: returns the dividend if divisor is zero.
-- Used by "IMP.Semantic.Expression" to implement modulo that doesn't raise exceptions.
(%%) :: Integer -> Integer -> Integer
(%%) v1 v2 = if v2 == 0 then v1 else mod v1 v2
