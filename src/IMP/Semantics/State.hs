{- |
Module      : IMP.Semantics.State
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
for debugging and analysis.
-}
module IMP.Semantics.State where

import Control.Monad.Trans.Except (ExceptT)
import System.Console.Haskeline

import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Result
import IMP.Syntax

-- | The REPL monad transformer stack.
type REPL = ExceptT Result (InputT IO)

-- | Map of defined Variables.
type Vars = Map.Map String Integer

-- | List of defined procedures.
type Procs = [Proc]

-- | The interpreter state: variables, procedures, break flag.
type State = (Vars, Procs, Bool)

-- | The initial interpreter state: no variables, no procedures, break flag unset.
initial :: State
initial = (Map.empty, [], False)

-- | Accessor for defined variables.
vrs :: State -> Vars
vrs (vars, _, _) = vars

-- | Accessor for defined procedures.
prs :: State -> Procs
prs (_, procs, _) = procs

-- | Accessor for break flag.
brk :: State -> Bool
brk (_, _, flag) = flag

-- | Execution trace: list of executed statements.
type Trace = [Stm]

-- | The REPL environment: state and execution trace.
type Env = (State, Trace)

-- | The initial REPL environment: initial state and empty trace.
start :: Env
start = (initial, [])

-- | Reset variables to empty map.
novars :: State -> State
novars (_, procs, flag) = (Map.empty, procs, flag)

-- | Reset Procedure definitions to empty list.
noprocs :: State -> State
noprocs (vars, _, flag) = (vars, [], flag)

-- | Reset break flag to False.
nobreak :: State -> State
nobreak (vars, procs, _) = (vars, procs, False)

-- | Get value of variable from state (zero if undefined).
getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | Set value of variable in state. The placeholder @_@ is ignored.
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar _ "" _ = error "variable name can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | Set multiple variables in state.
setVars :: State -> [(String, Integer)] -> State
setVars = foldl (uncurry . setVar)

-- | Get procedure by name from state.
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

-- | Set procedure in state.
setProc :: State -> Proc -> State
setProc (vars, procs, flag) proc = (vars, proc : procs, flag)

-- | Set break flag in state.
setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

-- | Reset break flag in state.
resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

-- | Generate variable name for flip index.
flipvar :: Integer -> String
flipvar i = "_flip" ++ show i

-- | Get value of flip variable (flip if zero, otherwise flop).
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | Set flip variable to flip.
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | Set flip variable to flop.
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | Program configuration with stack of states and remaining steps of statement.
type Conf = ([State], Maybe Stm)
