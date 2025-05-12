module IMP.Semantics.State where

import System.Console.Haskeline
import Control.Exception (Exception)
import Control.Monad.Trans.Except (ExceptT)

import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Syntax.Types
import IMP.Errors

data Throw = Throw Integer deriving (Show)
instance Exception Throw

type REPL = ExceptT Errors (InputT IO)

type Vars = Map.Map String Integer
type Procs = [Proc]
type State = (Vars, Procs, Bool)

initial :: State
initial = (Map.empty, [], False)

vrs :: State -> Vars
vrs (vars, _, _) = vars

prs :: State -> Procs
prs (_, procs, _) = procs

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
