{- |
Module      : IMP.State
Description : State for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

State implementation for the IMP language.
Provides definition and manipulation of state with some QOL helpers, and the
control flow outcome shared by both semantics.
-}
module IMP.State (
    Vars,
    Procs,
    State (..),
    Flow (..),
    flowState,
    onFlow,
    initial,
    getVar,
    lookupVar,
    setVar,
    setVars,
    restoreVar,
    resetVars,
    getProc,
    setProc,
    resetProcs,
    visible,
    flipvar,
    getFlip,
    setFlip,
    setFlop,
    (//),
    (%%),
) where

import qualified Data.Map.Strict as Map

import IMP.Syntax

-- | Map of defined variables from string identifier to integer values.
type Vars = Map.Map String Integer

-- | Map of defined procedures from name to procedure.
type Procs = Map.Map String Proc

-- | Interpreter state of defined variables and procedures.
data State = State
    { vars :: !Vars
    -- ^ Defined variables, undefined ones read as zero.
    , procs :: !Procs
    -- ^ Defined procedures.
    }
    deriving (Eq, Show)

-- | Control flow outcome of executing a statement.
data Flow
    = -- | Completed normally.
      Next !State
    | -- | Interrupted by @break@, propagating to the innermost enclosing loop.
      Broken !State
    deriving (Eq, Show)

-- | State of control flow outcome.
flowState :: Flow -> State
flowState (Next state) = state
flowState (Broken state) = state

-- | Map over state of control flow outcome.
onFlow :: (State -> State) -> Flow -> Flow
onFlow f (Next state) = Next (f state)
onFlow f (Broken state) = Broken (f state)

-- | Initial state with no variables or procedures defined.
initial :: State
initial = State Map.empty Map.empty

-- | Get integer value of provided variable identifier or zero if undefined.
getVar :: State -> String -> Integer
getVar state x = Map.findWithDefault 0 x (vars state)

-- | Get integer value of provided variable identifier, 'Nothing' if undefined.
lookupVar :: State -> String -> Maybe Integer
lookupVar state x = Map.lookup x (vars state)

-- | Set variable name to integer value.
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- INFO: Placeholder write-only variable
setVar _ "" _ = error "illegal argument for variable: identifier can't be empty string"
setVar state x v = state {vars = Map.insert x v (vars state)}

-- | Set list of variable names to their paired integer values.
setVars :: State -> [(String, Integer)] -> State
setVars = foldl $ uncurry . setVar

-- | Restore variable to previous value, or undefine it if there was none.
restoreVar :: State -> String -> Maybe Integer -> State
restoreVar state x = maybe (state {vars = Map.delete x (vars state)}) (setVar state x)

-- | Reset variable definitions.
resetVars :: State -> State
resetVars state = state {vars = Map.empty}

-- | Get some procedure by name, return 'Nothing' if undefined.
getProc :: State -> String -> Maybe Proc
getProc state name = Map.lookup name (procs state)

-- | Set procedure.
setProc :: State -> String -> Proc -> State
setProc state name proc = state {procs = Map.insert name proc (procs state)}

-- | Reset procedure definitions.
resetProcs :: State -> State
resetProcs state = state {procs = Map.empty}

-- | Whether variable is user visible, i.e. not internal like the @repeat@ counter.
visible :: String -> Bool
visible ('_' : _) = False
visible _ = True

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
