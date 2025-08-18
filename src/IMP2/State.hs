module IMP2.State where

import qualified Control.Monad.Trans.Except as Except
import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import System.IO
import Text.Read (readMaybe)

import Config

import IMP2.Exception
import IMP2.Syntax

-- | __TODO__
type IMP = Except.ExceptT Exception IO

-- | Map of defined variables from string identifier to integer values.
type Vars = Map.Map String Integer

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, [Proc], Bool)

-- | Interpreter configuration as pair of state stack and remaining statement execution.
type Conf = ([State], Maybe Stm)

-- | Default variable map with no definitions.
zero :: Map.Map String Integer
zero = Map.empty

-- | Initial state with no variable definitions, no procedure definitions and break flag unset.
initial :: State
initial = (zero, [], False)

-- | __TODO__
getVal :: String -> IMP Integer
getVal p = do
    liftIO $ putStr p >> hFlush stdout
    result <- Just <$> liftIO getLine
    case result of
        Nothing -> throwError Empty
        Just s -> case readMaybe s of
            Nothing -> do
                liftIO . print . Info $ "invalid input, please enter an integer"
                getVal p
            Just i -> return i

-- maybe undefined undefined (readMaybe result)

-- | Get the integer value of provided variable identifier or zero if undefined.
getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | __TODO__
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar _ "" _ = error "variable name can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | __TODO__
setVars :: State -> [(String, Integer)] -> State
setVars = foldl $ uncurry . setVar

-- | __TODO__
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

-- | __TODO__
setProc :: State -> Proc -> State
setProc (vars, procs, flag) proc = (vars, proc : procs, flag)

-- | __TODO__
getBreak :: State -> Bool
getBreak (_, _, flag) = flag

-- | __TODO__
setBreak :: State -> State
setBreak (vars, procs, _) = (vars, procs, True)

-- | __TODO__
resetBreak :: State -> State
resetBreak (vars, procs, _) = (vars, procs, False)

-- | __TODO__
flipvar :: Integer -> String
flipvar i = "_flip" ++ show i

-- | __TODO__
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | __TODO__
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | __TODO__
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | __TODO__
(//) :: Integer -> Integer -> Integer
(//) v1 v2 = if v2 == 0 then 0 else div v1 v2

-- | __TODO__
(%%) :: Integer -> Integer -> Integer
(%%) v1 v2 = if v2 == 0 then v1 else mod v1 v2
