{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Main
Description : Web entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web/WASM entrypoint for the IMP language interpreter.
This module implements the REPL loop without haskeline dependency.
Uses the polymorphic REPL monad from REPL.State with IO as the base monad.
-}
module Main where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import qualified System.Exit as Exit
import System.IO (hFlush, isEOF, stdout)

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.State

-- | Run the REPL with the given initial store
repl :: Store -> IO ()
repl store = do
    putStrLn (_welcome store)
    result <- runExceptT (execStateT loop store)
    case result of
        Left e -> print e >> Exit.exitFailure
        Right _ -> putStrLn goodbye

-- | Main REPL loop using basic IO
loop :: REPL IO ()
loop = do
    prompt' <- gets _prompt
    separator' <- gets _separator
    liftIO $ putStr (prompt' ++ separator' : " ") >> hFlush stdout

    eof <- liftIO isEOF
    if eof
        then return ()
        else do
            line <- liftIO getLine
            case line of
                "" -> loop
                ":)" -> liftIO (putStrLn "You look good today!") >> loop
                (':' : meta) ->
                    either
                        (const $ liftIO (putStrLn $ unlines ["unrecognized meta command: :" ++ meta, hint]) >> loop)
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
                liftIO . print $ evaluate aexp state
            Boolean bexp ->
                liftIO . putStrLn $ if evaluate bexp state then "true" else "false"
            Whitespace -> return ()

-- | Dispatcher for 'IMP.Meta.Command' with IO backend.
instance Dispatches IO Command where
    dispatch Quit = return ()
    dispatch command =
        case command of
            Help -> help
            Clear -> clear
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
        AssertFail _ -> throwError e -- irrecoverable, propagate
        Raised _ -> throwError e -- ''
        Info msg -> liftIO (putStrLn msg) >> loop -- informational message
        _ -> liftIO (print e) >> loop -- recoverable errors

-- | Clear the terminal (simple version for web).
clear :: REPL IO ()
clear = liftIO $ putStrLn $ replicate 50 '\n'

-- | Entrypoint for web/WASM IMP interpreter
main :: IO ()
main = repl start
