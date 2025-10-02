{-# LANGUAGE CPP #-}

{- |
Module      : Util
Description : Utility for 'REPL' and 'Web'
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides shared utility for the two modules 'REPL' and 'Web'.
-}
module Util where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.Console.Haskeline hiding (display)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import Meta
import Preset

-- | Encapsulation of computation in 'REPL'.
type REPL = StateT Store (ExceptT Exception (InputT IO))

-- | Lift computation from 'IMP.State.IMP' into 'REPL'.
liftIMP :: IMP a -> REPL a
liftIMP m = (lift . liftIO . runExceptT) m >>= either throwError return

-- | Encapsulation of state in 'REPL'.
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

-- | Starting data store for 'repl'.
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

-- | Reset 'IMP.Meta.Aspect'.
reset :: Aspect -> REPL ()
reset aspect = do
    state <- gets _state
    case aspect of
        All -> modify (\st -> st {_state = initial, _trace = []}) >> inform "environment reset"
        Vars -> modify (\st -> st {_state = resetVars state}) >> inform "variables reset"
        Procs -> modify (\st -> st {_state = resetProcs state}) >> inform "procedures reset"
        Flag -> modify (\st -> st {_state = resetBreak state}) >> inform "break flag reset"
        Trace -> modify (\st -> st {_trace = []}) >> inform "trace reset"

-- | Show 'IMP.Meta.Aspect'.
shower :: Aspect -> REPL ()
shower aspect = do
    (vars, procs, flag) <- gets _state
    trace <- gets _trace
    case aspect of
        All -> shower Vars >> shower Procs >> shower Flag >> shower Trace
        Vars ->
            explain
                "Variables:"
                -- INFO: invariant of IMP.State.setVar guarantees no empty string key
                [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        Procs -> explain "Procedures:" [prettify p | p <- procs]
        Flag -> outputln $ "Break: " ++ show flag ++ "\n"
        Trace ->
            explain
                "Trace:"
                [ init . unlines $ zipWith (++) (index : buffer) (lines s)
                | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
                , let
                    index = '#' : show i ++ space 2
                    buffer = repeat . space $ length (show i) + 3
                ]

-- | Interpret IMP language source file, return updated state.
loadIMP :: FilePath -> REPL ()
loadIMP path = do
    content <-
        liftIO (readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state <- gets _state
            trace <- gets _trace
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state', _trace = stm : trace}
    throwError . Info $ "interpreted: " ++ path

-- | Write trace to specified file.
writeIMP :: FilePath -> REPL ()
#ifdef javascript_HOST_OS

writeIMP path = gets _trace >>= liftIO . exportJS path . prettytrace

#else

writeIMP path = do
    content <- gets (prettytrace . _trace)
    _ <-
        liftIO (writeFile path content)
            `catchError` \e ->
                throwError . IOFail $
                    unlines ["write trace to: " ++ path, show e]
    throwError . Info $ "wrote trace to: " ++ path

#endif

-- | Export source code to new browser tab.
exportJS :: String -> FilePath -> IO ()
exportJS path code = undefined

-- | Show abstract syntax tree of 'IMP.Meta.Element'.
ast :: Element -> REPL ()
ast (Input construct) = display construct
ast (Index n) = do
    trace <- gets _trace
    if n <= 0 || n > length trace
        then errata $ "index out of bounds: " ++ show n
        else display (trace !! (length trace - n)) -- INFO: condition guarantees index in bounds

-- | Set 'IMP.Meta.Option'.
set :: Option -> REPL ()
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

-- | Nicely format 'outputln' with heading and indented body.
explain :: String -> [String] -> REPL ()
explain heading [] = outputln heading
explain heading body = outputln $ heading ++ '\n' : indent 4 (unlines body)

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
