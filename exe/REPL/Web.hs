{-# LANGUAGE TypeApplications #-}

{- |
Module      : REPL.Web
Description : Web REPL implementation using basic IO
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides REPL implementation for web/WASM context using basic IO.
This module implements the REPL loop without haskeline dependency.
Uses the polymorphic REPL monad from REPL.State with IO as the base monad.
-}
module REPL.Web (repl) where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.IO (hFlush, isEOF, stdout)
import qualified System.Exit as Exit

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
            processLine line

-- | Process a single line of input
processLine :: String -> REPL IO ()
processLine "" = loop
processLine ":)" = liftIO (putStrLn "You look good today!") >> loop
processLine (':':meta) = do
    case parser "meta" meta of
        Left _ -> do
            liftIO $ putStrLn $ unlines ["unrecognized meta command: :" ++ meta, hint]
            loop
        Right cmd -> (dispatchCommand cmd >> loop) `catchError` handleREPLError
processLine input = do
    case parser @Construct "interactive" input of
        Left e -> do
            liftIO . putStrLn . unlines $ [input, show e]
            loop
        Right construct -> (dispatchConstruct construct >> loop) `catchError` handleREPLError

-- | Handle REPL errors
handleREPLError :: Exception -> REPL IO ()
handleREPLError e = case e of
    Empty -> return ()  -- EOF, exit cleanly
    AssertFail _ -> throwError e  -- irrecoverable
    Raised _ -> throwError e  -- irrecoverable
    Info msg -> liftIO (putStrLn msg) >> loop  -- informational message
    _ -> liftIO (print e) >> loop  -- recoverable errors

-- | Dispatch IMP construct
dispatchConstruct :: Construct -> REPL IO ()
dispatchConstruct construct = do
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

-- | Dispatch meta command (uses functions from REPL.State)
dispatchCommand :: Command -> REPL IO ()
dispatchCommand Quit = return ()
dispatchCommand command = case command of
    Help -> help
    Clear -> liftIO $ putStrLn $ replicate 50 '\n'  -- Simple clear for web
    Version -> version
    Reset aspect -> reset aspect
    Show aspect -> shower aspect
    Load path -> loadIMP path
    Write path -> writeIMP path
    AST element -> ast element
    Set option -> set option
