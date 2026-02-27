{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : REPL.Execute.Browser
Description : Browser-based REPL execution backend
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASM/JavaScript

Browser-based execution backend for the IMP language REPL.
Implements 'Dispatches' instances for 'IO' monad to handle REPL commands, constructs,
and exceptions in the browser environment via JavaScript FFI.
-}
module REPL.Execute.Browser where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Data.IORef
import GHC.Wasm.Prim

import qualified System.Exit as Exit

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.State hiding (writeIMP)

-- | Clear terminal screen and write welcome message
foreign import javascript unsafe "globalThis.impli.writeWelcome()" js_writeWelcome :: IO ()

-- | Write IMP trace to plaintext file in new browser tab
foreign import javascript unsafe "globalThis.impli.writeTrace($1, $2)" js_writeIMP :: JSString -> JSString -> IO ()

-- | Write to JS console
foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()

logger :: String -> IO ()
logger = js_log . toJSString

-- | Run the REPL with the given initial store
repl :: Store -> IO ()
repl store = do
    putStrLn "Welcome to the IMP REPL! Enter :help to list available metacommands."
    result <- runExceptT (execStateT loop store)
    case result of
        Left e -> print e >> Exit.exitFailure
        Right _ -> do
            putStrLn goodbye
            logger "You will never leave this place!"
            repl start -- escape is impossible

-- | Main REPL loop using basic IO
loop :: REPL IO ()
loop = do
    prompt' <- gets _prompt
    separator' <- gets _separator
    action <- liftIO $ readIORef inputter
    line <- liftIO $ action (prompt' ++ separator' : " ")
    case line of
        "" -> loop
        ":)" -> outputln "You look good today!" >> loop
        (':' : meta) ->
            either
                (const . errata $ unlines ["unrecognized meta command: :" ++ meta, hint])
                (dispatch @IO @Command)
                (parser "meta" meta)
        input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch @IO @Construct c >> loop)
                (parser "interactive" input)
        `catchError` dispatch @IO @Exception

-- | Dispatcher for 'IMP.Syntax.Construct' with IO backend.
instance Dispatches IO Construct where
    dispatch construct = do
        trace <- gets _trace
        state <- gets _state
        case construct of
            Statement stm -> do
                state' <- liftIMP $ execute (stm, state)
                modify $ \st -> st {_state = state', _trace = stm : trace}
            Arithmetic aexp ->
                display $ evaluate aexp state
            Boolean bexp ->
                outputln $ if evaluate bexp state then "true" else "false"
            Whitespace -> return ()

-- | Dispatcher for 'IMP.Meta.Command' with IO backend.
instance Dispatches IO Command where
    dispatch Quit = return ()
    dispatch command =
        case command of
            Help ->
                explain
                    "All metacommands besides :(un)set can be abbreviated by their first letter"
                    [ ":help                    Show this help message"
                    , ":quit                    Quit REPL"
                    , ":clear                   Clear screen"
                    , ":version                 Show version"
                    , ":set OPTION VALUE        Set REPL option (welcome, prompt, separator, goodbye, verbose)"
                    , ":unset OPTION            Unset REPL option"
                    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
                    , ":show [ASPECT]           Show environment or specific aspect"
                    , ":load FILE               Interpret file and load resulting state"
                    , ":write FILE              Write trace to plaintext file in new browser tab"
                    , ":ast (INPUT | #n)        Parse and display AST of input or n-th statement in trace"
                    ]
            Clear -> liftIO js_writeWelcome
            Version -> version
            Reset aspect -> reset aspect
            Show aspect -> shower aspect
            Load path -> loadIMP path
            Write path -> writeIMP path
            AST element -> ast element
            Set option -> set option
            >> loop

-- | Dispatcher for 'IMP.Exception.Exception' with IO backend.
instance Dispatches IO Exception where
    dispatch e = case e of
        Empty -> return () -- EOF, exit cleanly
        AssertFail _ -> display e -- irrecoverable - display, die, respawn
        Raised _ -> display e -- ''
        _ -> display e >> loop -- recoverable errors

-- | Write trace to new browser tab as plaintext file.
writeIMP :: String -> REPL IO ()
writeIMP path = do
    content <- gets (prettytrace . _trace)
    let
        js_path = toJSString path
        js_content = toJSString content
    liftIO $ js_writeIMP js_path js_content
    inform "wrote trace to new tab"
