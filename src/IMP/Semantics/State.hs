{- |
Module      : IMP.Semantics.State
Description : Defines the state and environment for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides the definitions for managing the state and environment
of `impli`. It includes types and functions for handling variables, procedures,
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

type REPL = ExceptT Result (InputT IO)

type Vars = Map.Map String Integer
type Procs = [Proc]
type State = (Vars, Procs, Bool)

initial :: State
initial = (Map.empty, [], False)

vrs :: State -> Vars
vrs (vars, _, _) = vars

prs :: State -> Procs
prs (_, procs, _) = procs

type Trace = [Stm]
type Env = (State, Trace)

start :: Env
start = (initial, [])

st :: Env -> State
st = fst

tr :: Env -> Trace
tr = snd

brk :: State -> Bool
brk (_, _, flag) = flag

getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar (vars, procs, flag) x v = (Map.insert x v vars, procs, flag)

setVars :: State -> [(String, Integer)] -> State
setVars = foldl (uncurry . setVar)

getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

setProc :: State -> Proc -> State
setProc (vars, procs, flag) p = (vars, p : procs, flag)

setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

flipvar :: Integer -> String
flipvar i = "_" ++ show i

getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1
