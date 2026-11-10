{- |
Module      : IMP.State
Description : State for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

State implementation for the IMP language.
Provides definition and manipulation of state with some QOL helpers.
Entirely pure: interpretation effects are reified by the semantics modules
in @IMP.Semantics@ and discharged by frontends through @IMP.Engine@ handlers.
-}
module IMP.State where

import qualified Data.Map as Map

import IMP.Syntax

-- | Tuple of statement and state, the machine configuration between steps.
type Gamma = (Stm, State)

-- | Map of defined variables from string identifier to integer values.
type Vars = Map.Map String Integer

-- | Map of defined procedures from name to procedure.
type Procs = Map.Map String Proc

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, Procs, Bool)

-- | Initial state with no variable definitions, no procedures defined and break flag unset.
initial :: State
initial = (Map.empty, Map.empty, False)

-- | Get defined variables.
getVars :: State -> Vars
getVars (vars, _, _) = vars

-- | Get integer value of provided variable identifier or zero if undefined.
getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | Set variable name to integer value.
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- INFO: Placeholder write-only variable
setVar _ "" _ = error "illegal argument for variable: identifier can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | Set list of variable names to their paired integer values.
setVars :: State -> [(String, Integer)] -> State
setVars = foldl $ uncurry . setVar

-- | Reset variable definitions.
resetVars :: State -> State
resetVars (_, procs, flag) = (Map.empty, procs, flag)

-- | Get defined procedures.
getProcs :: State -> Procs
getProcs (_, procs, _) = procs

-- | Get some procedure by name, return 'Nothing' if undefined.
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = Map.lookup name procs

-- | Set procedure.
setProc :: State -> String -> Proc -> State
setProc (vars, procs, flag) name proc =
    (vars, Map.insert name proc procs, flag)

-- | Reset procedure definitions.
resetProcs :: State -> State
resetProcs (vars, _, flag) = (vars, Map.empty, flag)

-- | Get break flag.
getBreak :: State -> Bool
getBreak (_, _, flag) = flag

-- | Set break flag to 'True'.
setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

-- | Set break flag to 'False'.
resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

-- | Indicator variable for corresponding flip-flop index for internal use.
flipvar :: Integer -> String
flipvar i = "_flip_" ++ show i

-- | Check if branch to flip.
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | Set branch to flip.
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | Set branch to flop.
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | Safe division, zero if divisor is zero.
(//) :: Integer -> Integer -> Integer
(//) _ 0 = 0
(//) v1 v2 = div v1 v2

-- | Safe modulus, dividend if divisor is zero.
(%%) :: Integer -> Integer -> Integer
(%%) v1 0 = v1
(%%) v1 v2 = mod v1 v2
