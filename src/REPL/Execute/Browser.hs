{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : REPL.Execute.Browser
Description : WASM-friendly REPL execution backend
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

WASM-friendly execution backend for the IMP language REPL.
Uses plain IO streams so the same runtime can be hosted in browsers and by
WASI runtimes such as Wasmtime.
-}
module REPL.Execute.Browser where

import Control.Exception (IOException, try)
import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Data.IORef

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

-- | Run the REPL with the given initial store.
repl :: Store -> IO ()
repl store = do
    putStrLn $ _welcome store
    result <- runExceptT (execStateT loop store)
    case result of
        Left e -> print e
        Right st -> putStrLn (_goodbye st)

-- | Main REPL loop using basic IO.
loop :: REPL IO ()
loop = do
    prompt' <- gets _prompt
    separator' <- gets _separator
    action <- liftIO $ readIORef inputter
    line <- liftIO $ action (prompt' ++ separator' : " ")
    case line of
        "\EOT" -> throwError Empty -- Ctrl-D, EOF
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
            Help -> help
            Clear -> output "\ESC[2J\ESC[H"
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
        Empty -> return ()
        AssertFail _ -> display e
        Raised _ -> display e
        _ -> display e >> loop

-- | Write trace to file for WASI/native-like environments.
writeIMP :: String -> REPL IO ()
writeIMP path = do
    content <- gets (prettytrace . _trace)
    result <- liftIO (try (writeFile path content) :: IO (Either IOException ()))
    either
        (\e -> throwError . IOFail $ unlines ["write trace to: " ++ path, show (e :: IOException)])
        (\_ -> return ())
        result
    inform $ "wrote trace to: " ++ path
