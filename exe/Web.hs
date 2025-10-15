{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Main
Description : Web capability for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web capabilities for the IMP language interpreter.
-}
module Main where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Language.Javascript.JSaddle.String (JSString)
import System.Console.Haskeline hiding (display)
import System.Exit (exitFailure)

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.Util (
    REPL,
    Store (..),
    ast,
    clear,
    help,
    liftIMP,
    loadIMP,
    reset,
    set,
    shower,
    start,
    version,
 )

foreign import javascript unsafe "console.log($1)" logger :: JSString -> IO ()

-- | Entrypoint for the IMP language interpreter in the web.
main :: IO ()
main = do
    logger "Hello from Haskel!"
    repl start

-- | Read-Evaluate-Print-Loop in the 'REPL' monad.
repl :: Store -> IO ()
repl store = do
    putStrLn $ _welcome store
    runInputTWithPrefs
        defaultPrefs
        defaultSettings
        (withInterrupt (runExceptT (execStateT loop store)))
        >>= either (\e -> print e >> exitFailure) (\store' -> putStrLn goodbye >> repl store')
    error "How did we get here?"

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL ()
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
                (dispatch @Command)
                (parser "meta" meta)
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch @Construct c >> loop)
                (parser "interactive" input)
        `catchError` \e -> case e of
            Empty -> return () -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- irrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop -- mistakes happen

-- | Typeclass to dispatch 'IMP.Syntax.Construct' or 'IMP.Meta.Command'.
class Dispatches a where
    -- | Dispatch execution.
    dispatch :: (Parses a) => a -> REPL ()

-- | Dispatcher for 'IMP.Syntax.Construct'.
instance Dispatches Construct where
    dispatch construct = do
        trace <- gets _trace
        state <- gets _state
        case construct of
            Statement stm -> do
                state' <- liftIMP $ execute (stm, state)
                modify $ \st -> st {_state = state', _trace = stm : trace}
            Arithmetic aexp -> display (evaluate state aexp)
            Boolean bexp -> outputln (if evaluate state bexp then "true" else "false")
            Whitespace -> return ()

-- | Dispatcher for 'IMP.Meta.Command'.
instance Dispatches Command where
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

-- | Write trace to specified file.
writeIMP :: FilePath -> REPL ()
writeIMP path = gets _trace >>= liftIO . exportJS path . prettytrace

-- | Export source code to new browser tab.
exportJS :: String -> FilePath -> IO ()
exportJS path code = undefined
