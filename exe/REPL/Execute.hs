{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : REPL.Execute
Description : Read-Evaluate-Print-Loop for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the Read-Evaluate-Print-Loop for the IMP language interpreter.
This includes interactive parsing into 'IMP.Syntax.Construct' followed by interpretation
with 'IMP.Expression.evaluate' or 'IMP.Statement.execute'.
Supports various metacommands, such as inspection of interpreter state, interpret source file,
print AST of IMP language construct and save execution history to disk.
-}
module REPL.Execute where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.Console.Haskeline hiding (display)
import System.Exit (exitFailure)

import qualified System.Console.ANSI as ANSI

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.State

-- | Type alias for haskeline-based IO.
type IO' = InputT IO

-- | Encapsulation of REPL customization.
data Setup = Setup
    { settings :: Settings IO
    , prefs :: Prefs -- INFO: for more information visit https://github.com/haskell/haskeline/wiki/UserPreferences
    }

-- | Setup with arguments.
setup :: Maybe FilePath -> Maybe FilePath -> IO Setup
setup Nothing Nothing = return Setup {settings = defaultSettings, prefs = defaultPrefs}
setup hist conf = do
    let settings' = defaultSettings {historyFile = hist}
    prefs' <- maybe (return defaultPrefs) readPrefs conf
    return Setup {settings = settings', prefs = prefs'}

-- | Read-Evaluate-Print-Loop in the 'REPL' monad.
repl :: Setup -> Store -> IO ()
repl (Setup s p) store = do
    putStrLn $ _welcome store
    runInputTWithPrefs p s (withInterrupt (runExceptT (execStateT loop store)))
        >>= either (\e -> print e >> exitFailure) (\_ -> putStrLn goodbye)

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL IO' ()
loop = handleInterrupt loop $ do
    prompt' <- gets _prompt
    separator' <- gets _separator
    line <- lift . lift . getInputLine $ prompt' ++ separator' : " "
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just ":)" -> outputln "You look good today!" >> loop -- it's true
        Just (':' : meta) ->
            either
                (const . errata $ unlines ["unrecognized meta command: :" ++ meta, hint])
                (dispatch @IO' @Command)
                (parser "meta" meta)
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch @IO' @Construct c >> loop)
                (parser "interactive" input)
        `catchError` dispatch @IO' @Exception

-- | Dispatcher for 'IMP.Syntax.Construct' with haskeline backend.
instance Dispatches IO' Construct where
    dispatch construct = do
        trace <- gets _trace
        state <- gets _state
        case construct of
            Statement stm -> do
                state' <- liftIMP $ execute (stm, state)
                modify $ \st -> st {_state = state', _trace = stm : trace}
            Arithmetic aexp -> display (evaluate aexp state)
            Boolean bexp -> outputln (if evaluate bexp state then "true" else "false")
            Whitespace -> return ()

-- | Dispatcher for 'IMP.Meta.Command' with haskeline backend.
instance Dispatches IO' Command where
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

-- | Dispatcher for 'IMP.Exception.Exception' with haskeline backend.
instance Dispatches IO' Exception where
    dispatch e = case e of
        Empty -> return () -- ctrl-d during read, flush line and exit cleanly
        AssertFail _ -> throwError e -- irrecoverable, propagate
        Raised _ -> throwError e -- ''
        _ -> display e >> loop -- mistakes happen

-- | Clear the terminal (haskeline-specific).
clear :: REPL IO' ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)
