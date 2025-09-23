{-# LANGUAGE FlexibleContexts #-}

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
-}
module IMP.State where

import Control.Exception (IOException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import System.IO
import Text.Read (readMaybe)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Exception
import IMP.Syntax

-- | Encapsulation of computation in 'IMP.Semantics'.
type IMP = Except.ExceptT Exception IO

-- | Map of defined variables from string identifier to integer values.
type Vars = Map.Map String Integer

-- | Interpreter state as triple of defined variables, procedures and break flag.
type State = (Vars, [Proc], Bool)

-- | Interpreter configuration as 2-tuple of remaining statement execution and state stack.
type Conf = (Maybe Stm, [State])

-- | Default variable map with no definitions.
zero :: Map.Map String Integer
zero = Map.empty

-- | Initial state with no variable definitions, no procedure definitions and break flag unset.
initial :: State
initial = (zero, [], False)

-- | Get value for provided variable with prompt.
getVal :: String -> IMP Integer
getVal x = do
    output (x ++ " := ") >> flush
    input <-
        liftIO (try getLine :: IO (Either IOException String))
            >>= either (\_ -> throwError Empty) return
    case readMaybe input of
        Nothing -> do
            display . Info $ "invalid input, please enter an integer"
            getVal x
        Just i -> return i
    where
        flush = liftIO $ hFlush stdout

-- | Get defined variables.
getVars :: State -> Vars
getVars (vars, _, _) = vars

-- | Get integer value of provided variable identifier or zero if undefined.
getVar :: State -> String -> Integer
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

-- | Set variable name to integer value.
setVar :: State -> String -> Integer -> State
setVar state "_" _ = state -- INFO: placeholder write-only variable
setVar _ "" _ = error "illegal argument for variable: identifier can't be empty string"
setVar (vars, procs, flag) var val = (Map.insert var val vars, procs, flag)

-- | Set list of variable names to their paired integer values.
setVars :: State -> [(String, Integer)] -> State
setVars = foldl $ uncurry . setVar

-- | Reset variable definitions.
resetVars :: State -> State
resetVars (_, procs, flag) = (Map.empty, procs, flag)

-- | Get defined procedures.
getProcs :: State -> [Proc]
getProcs (_, procs, _) = procs

-- | Get some procedure by name, return 'Nothing' if undefined.
getProc :: State -> String -> Maybe Proc
getProc (_, procs, _) name = List.find ((name ==) . procname) procs

-- | Set procedure.
setProc :: State -> Proc -> State
setProc (vars, procs, flag) proc = (vars, proc : procs, flag)

-- | Reset procedure definitions.
resetProcs :: State -> State
resetProcs (vars, _, flag) = (vars, [], flag)

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
flipvar i = "_flip" ++ show i

-- | Check if branch to flip.
getFlip :: State -> Integer -> Bool
getFlip state i = getVar state (flipvar i) == 0

-- | Set branch to flip.
setFlip :: State -> Integer -> State
setFlip state i = setVar state (flipvar i) 0

-- | Set branch to flop.
setFlop :: State -> Integer -> State
setFlop state i = setVar state (flipvar i) 1

-- | 'putStr' inside some IO Monad.
output :: (MonadIO m) => String -> m ()
output = liftIO . putStr

-- | 'putStrLn' inside some IO Monad.
outputln :: (MonadIO m) => String -> m ()
outputln = liftIO . putStrLn

-- | 'print' inside some IO Monad.
display :: (MonadIO m, Show a) => a -> m ()
display = liftIO . print

-- | Safe division, zero if divisor is zero.
(//) :: Integer -> Integer -> Integer
(//) _ 0 = 0
(//) v1 v2 = div v1 v2

-- | Safe modulus, dividend if divisor is zero.
(%%) :: Integer -> Integer -> Integer
(%%) v1 0 = v1
(%%) v1 v2 = mod v1 v2

-- | Convenience function for error and information passing.
errata, inform :: (MonadError Exception m) => String -> m a
errata = throwError . Error
inform = throwError . Info
