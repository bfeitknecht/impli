{- |
Module      : IMP.Semantic.State
Description : Defines the state and environment for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides the definitions for managing the state and environment
of @impli@. It includes types and functions for handling variables, procedures,
and execution traces, as well as utility functions for state manipulation.
The state is represented as a combination of defined variables, procedures,
and a flag for break conditions. The environment includes the execution trace
for debugging and analysis. This module is used throughout "IMP.Semantic.Statement" and
"IMP.Semantic.Expression" to manipulate program state during interpretation.
-}
module IMP.Semantic.State (
    REPL,
    Conf,
    Vars,
    State,
    Trace,
    Env,
    initial,
    start,
    vrs,
    prs,
    brk,
    novars,
    noprocs,
    nobreak,
    getVar,
    setVar,
    setVars,
    getProc,
    setProc,
    setBreak,
    resetBreak,
    getFlip,
    setFlip,
    setFlop,
    output,
    display,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Console.Haskeline as Haskeline

import IMP.Result
import IMP.Syntax
import IMP.Util

-- | The REPL monad transformer stack.
-- Used by "IMP.REPL" for interactive sessions.
type REPL = ExceptT Result (Haskeline.InputT IO)

-- | Map of defined Variables. Maps variable names to their integer values.
type Vars = Map.Map String Integer

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, [Proc], Bool)

-- | Program configuration with stack of states and remaining steps of statement.
-- Used by the small-step interpreter in "IMP.Semantic.Statement" to represent execution state.
type Conf = ([State], Maybe Stm)

-- | Initial interpreter state with no variables, no procedures and break flag unset.
-- Used when starting a new interpreter session in "IMP.REPL".
initial :: State
initial = (Map.empty, [], False)

-- | Accessor for defined variables. Extracts the variable map from a state.
vrs :: State -> Vars
vrs (vars, _, _) = vars

-- | Accessor for defined procedures. Extracts the procedure list from a state.
prs :: State -> [Proc]
prs (_, procs, _) = procs

-- | Accessor for break flag. Extracts the break flag from a state.
brk :: State -> Bool
brk (_, _, flag) = flag

-- | Execution trace as list of executed statements.
-- Used for debugging and history.
type Trace = [Stm]

-- | REPL environment as pair of state and trace.
-- Used by "IMP.REPL" to track current interpreter state and history of executed statements.
type Env = (State, Trace)

-- | Initial REPL environment with initial state and empty trace.
-- Used when starting a new REPL session in "IMP.REPL".
start :: Env
start = (initial, [])

-- | Reset variables to empty map.
-- Used by the @:reset vars@ metacommand in "IMP.REPL".
novars :: State -> State
novars (_, procs, flag) = (Map.empty, procs, flag)

-- | Reset Procedure definitions to empty list.
-- Used by the @:reset procs@ metacommand in "IMP.REPL".
noprocs :: State -> State
noprocs (vars, _, flag) = (vars, [], flag)

-- | Reset break flag to False.
-- Used by the @:reset break@ command in "IMP.REPL".
nobreak :: State -> State
nobreak (vars, procs, _) = (vars, procs, False)

-- | Get value of variable from state (zero if undefined).
-- Used by "IMP.Semantic.Expression" when evaluating variable references.
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

-- | Output string to the user, followed by newline and flush.
-- Used by "IMP.REPL" and "IMP.Semantic.Statement" for user interaction.
output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

-- | Display argument using its 'Show' instance.
-- Used by "IMP.REPL" to present structured output to users.
display :: (Show a) => a -> REPL ()
display = output . show
