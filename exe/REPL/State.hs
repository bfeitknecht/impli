{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : REPL.State
Description : Shared state and utilities for REPL modules
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides shared state, store definitions, and parametric utility functions for REPL modules.
Similar to IMP.State, this module contains all the core REPL functionality in a monad-parametric way.
-}
module REPL.State where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Data.Version (showVersion)

import qualified Data.Map as Map
import qualified Paths_impli as Paths

import IMP.Exception
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset

-- | Encapsulation of state in REPL.
data Store = Store
    { _state :: State
    , _trace :: [Stm]
    , _defaults :: Defaults
    , _welcome :: String
    , _prompt :: String
    , _separator :: Char
    , _goodbye :: String
    , _verbose :: Level
    }

-- | Starting data store for REPL.
start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , _defaults = defaults
        , _welcome = welcome
        , _prompt = prompt
        , _separator = normalsep
        , _goodbye = goodbye
        , _verbose = verbosity
        }

-- | Lift computation from 'IMP.State.IMP' into any REPL monad.
liftIMP :: (MonadIO m, MonadError Exception m) => IMP a -> m a
liftIMP m = (liftIO . runExceptT) m >>= either throwError return

-- | Reset 'IMP.Meta.Aspect' - parametric version.
reset :: (MonadState Store m, MonadIO m) => Aspect -> m ()
reset aspect = do
    state <- gets _state
    case aspect of
        All -> modify (\st -> st {_state = initial, _trace = []}) >> liftIO (putStrLn "environment reset")
        Vars -> modify (\st -> st {_state = resetVars state}) >> liftIO (putStrLn "variables reset")
        Procs -> modify (\st -> st {_state = resetProcs state}) >> liftIO (putStrLn "procedures reset")
        Flag -> modify (\st -> st {_state = resetBreak state}) >> liftIO (putStrLn "break flag reset")
        Trace -> modify (\st -> st {_trace = []}) >> liftIO (putStrLn "trace reset")

-- | Show 'IMP.Meta.Aspect' - parametric version.
shower :: (MonadState Store m, MonadIO m) => Aspect -> m ()
shower aspect = do
    (vars, procs, flag) <- gets _state
    trace <- gets _trace
    case aspect of
        All -> shower Vars >> shower Procs >> shower Flag >> shower Trace
        Vars ->
            explain
                "Variables:"
                [k ++ " = " ++ show v | (k, v) <- Map.toList vars, case k of [] -> False; (c:_) -> c /= '_']
        Procs -> explain "Procedures:" [prettify p | p <- procs]
        Flag -> liftIO $ putStrLn $ "Break: " ++ show flag ++ "\n"
        Trace ->
            explain
                "Trace:"
                [ init . unlines $ zipWith (++) (index : buffer) (lines s)
                | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
                , let
                    index = '#' : show i ++ space 2
                    buffer = repeat . space $ length (show i) + 3
                ]

-- | Interpret IMP language source file - parametric version.
loadIMP :: (MonadState Store m, MonadIO m, MonadError Exception m) => FilePath -> m ()
loadIMP path = do
    content <- liftIO (readFile path) `catchError` 
        (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state <- gets _state
            trace <- gets _trace
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state', _trace = stm : trace}
    throwError . Info $ "interpreted: " ++ path

-- | Write trace to specified file - parametric version.
writeIMP :: (MonadState Store m, MonadIO m, MonadError Exception m) => FilePath -> m ()
writeIMP path = do
    content <- gets (prettytrace . _trace)
    liftIO (writeFile path content) `catchError` 
        (\e -> throwError . IOFail $ unlines ["write trace to: " ++ path, show e])
    throwError . Info $ "wrote trace to: " ++ path

-- | Show abstract syntax tree of 'IMP.Meta.Element' - parametric version.
ast :: (MonadState Store m, MonadIO m, MonadError Exception m) => Element -> m ()
ast (Input construct) = liftIO $ print construct
ast (Index n) = do
    trace <- gets _trace
    if n <= 0 || n > length trace
        then liftIO $ putStrLn $ "index out of bounds: " ++ show n
        else liftIO $ print (trace !! (length trace - n))

-- | Set 'IMP.Meta.Option' - parametric version.
set :: (MonadState Store m) => Option -> m ()
set option = case option of
    Welcome w -> modify $ \st -> st {_welcome = w}
    Prompt p -> modify $ \st -> st {_prompt = p}
    Separator s -> modify $ \st -> st {_separator = s}
    Goodbye g -> modify $ \st -> st {_goodbye = g}
    Verbose v -> modify $ \st -> st {_verbose = v, _separator = separator'}
        where
            separator' = case v of
                Normal -> normalsep
                Profile -> profilesep
                Debug -> debugsep

-- | Nicely format output with heading and indented body - parametric version.
explain :: (MonadIO m) => String -> [String] -> m ()
explain heading [] = liftIO $ putStrLn heading
explain heading body = liftIO $ putStrLn $ heading ++ '\n' : indent 4 (unlines body)

-- | Explain the metacommands - parametric version.
help :: (MonadIO m) => m ()
help =
    explain
        "All metacommands besides :(un)set can be abbreviated by their first letter"
        helpMessage

-- | Output the version - parametric version.
version :: (MonadIO m) => m ()
version = liftIO $ putStrLn $ unwords ["impli", showVersion Paths.version]

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
