{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : REPL.Execute.Browser
Description : Browser-based line execution backend
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASM/JavaScript

Line-at-a-time browser backend for the IMP language REPL.
JavaScript drives prompt/input and calls into this module for evaluation.
-}
module REPL.Execute.Browser where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import GHC.Wasm.Prim

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

-- | Clear terminal screen and write welcome message.
foreign import javascript unsafe "globalThis.impliWriteWelcome()" js_writeWelcome :: IO ()

-- | Write IMP trace to plaintext file in new browser tab.
foreign import javascript unsafe "globalThis.impliWriteTrace($1, $2)" js_writeIMP :: JSString -> JSString -> IO ()

-- | Run one REPL line. Returns 'False' if the REPL should terminate.
runLine :: String -> REPL IO Bool
runLine line =
    eval line `catchError` \e -> dispatch @IO @Exception e >> return True
    where
        eval input = case input of
            "\EOT" -> return False
            "" -> return True
            ":)" -> outputln "You look good today!" >> return True
            (':' : meta) ->
                either
                    (const . errata $ unlines ["unrecognized meta command: :" ++ meta, hint])
                    command
                    (parser "meta" meta)
            _ ->
                either
                    (\e -> throwError . ParseFail $ unlines [input, show e])
                    (\c -> dispatch @IO @Construct c >> return True)
                    (parser "interactive" input)
        command Quit = outputln goodbye >> return False
        command cmd = dispatch @IO @Command cmd >> return True

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
                    , ":tips                    Show some tips"
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

-- | Dispatcher for 'IMP.Exception.Exception' with IO backend.
instance Dispatches IO Exception where
    dispatch e = case e of
        Empty -> return ()
        AssertFail _ -> display e
        Raised _ -> display e
        _ -> display e

-- | Write trace to new browser tab as plaintext file.
writeIMP :: String -> REPL IO ()
writeIMP path = do
    content <- gets (prettytrace . _trace)
    let
        js_path = toJSString path
        js_content = toJSString content
    liftIO $ js_writeIMP js_path js_content
    inform "wrote trace to new tab"
