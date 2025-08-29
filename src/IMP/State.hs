{- |
Module      : IMP.State
Description : TODO
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.State where

import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import System.IO
import Text.Read (readMaybe)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Exception
import IMP.Syntax

-- | Encapsulation of computation in 'IMP.Statement.interpret'.
type IMP = Except.ExceptT Exception IO

-- | Map of defined variables from string identifier to integer values.
type Vars = Map.Map String Integer

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, [Proc], Bool)

-- | Interpreter configuration as pair of state stack and remaining statement execution.
type Conf = (Maybe Stm, [State])

-- | Default variable map with no definitions.
zero :: Map.Map String Integer
zero = Map.empty

-- | Initial state with no variable definitions, no procedure definitions and break flag unset.
initial :: State
initial = (zero, [], False)

-- | TODO
getVal :: String -> IMP Integer
getVal x = do
    liftIO $ putStr (x ++ " := ") >> hFlush stdout
    input <-
        liftIO (try getLine :: IO (Either IOException String))
            >>= either (\_ -> throwError Empty) return
    case readMaybe input of
        Nothing -> do
            liftIO . print . Info $ "invalid input, please enter an integer"
            getVal x
        Just i -> return i

-- | TODO
getVars :: State -> Vars
getVars (vars, _, _) = vars

-- | Get the integer value of provided variable identifier or zero if undefined.
getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | TODO
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- INFO: placeholder write-only variable
setVar _ "" _ = error "illegal argument for variable: identifier can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | TODO
setVars :: State -> [(String, Integer)] -> State
setVars = foldl $ uncurry . setVar

-- | TODO
getProcs :: State -> [Proc]
getProcs (_, procs, _) = procs

-- | TODO
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

-- | TODO
setProc :: State -> Proc -> State
setProc (vars, procs, flag) proc = (vars, proc : procs, flag)

-- | TODO
getBreak :: State -> Bool
getBreak (_, _, flag) = flag

-- | TODO
setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

-- | TODO
resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

-- | TODO
flipvar :: Integer -> String
flipvar i = "_flip" ++ show i

-- | TODO
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | TODO
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | TODO
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | TODO
(//) :: Integer -> Integer -> Integer
(//) v1 v2 = if v2 == 0 then 0 else div v1 v2

-- | TODO
(%%) :: Integer -> Integer -> Integer
(%%) v1 v2 = if v2 == 0 then v1 else mod v1 v2
