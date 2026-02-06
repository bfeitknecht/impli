{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
module REPL.Execute.Browser where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Data.IORef
import GHC.Wasm.Prim
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (getLine, print, putStr, putStrLn)

import qualified System.Exit as Exit
import qualified Prelude

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset
import REPL.State hiding (writeIMP)

-- | Read input from JavaScript (awaits promise from @impli.readIn()@)
foreign import javascript safe "await globalThis.impli.readInput()" js_readInput :: IO JSString

-- | Write IMP trace to plaintext blob in new browser tab
foreign import javascript unsafe "globalThis.impli.writeIMP($1)" js_writeIMP :: JSString -> IO ()

-- | Clear terminal screen and write welcome message
foreign import javascript unsafe "globalThis.impli.writeWelcome()" js_writeWelcome :: IO ()

-- | Prompt to display before user input in terminal (exported to JS)
foreign export javascript "getPrompt" getPrompt :: JSString

-- | Get the current prompt from the REPL state (prompt + separator + space)
getPrompt :: JSString
getPrompt = toJSString prompts
    where
        store = unsafePerformIO (readIORef ref)
        prompts = _prompt store ++ [_separator store] ++ " "

-- | Get line from terminal via JSFFI
getLine :: IO String
getLine = fromJSString <$> js_readInput

-- | Put string to terminal via WASI output
putStr :: String -> IO ()
putStr = Prelude.putStr

-- | Put string with newline to terminal via WASI output
putStrLn :: String -> IO ()
putStrLn = Prelude.putStrLn

-- | Print to terminal via WASI output
print :: (Show a) => a -> IO ()
print = Prelude.print

-- | Never EOF in browser context
isEOF :: IO Bool
isEOF = return False

-- | Global IORef to store the current REPL state
{-# NOINLINE ref #-}
ref :: IORef Store
ref = unsafePerformIO (newIORef start)

-- | Run the REPL with the given initial store
repl :: Store -> IO ()
repl store = do
    writeIORef ref store -- Initialize the global store
    js_writeWelcome
    result <- runExceptT (execStateT loop store)
    case result of
        Left e -> print e >> Exit.exitFailure
        Right final -> do
            writeIORef ref final -- Update global store
            putStrLn goodbye >> repl start -- escape is impossible -- TODO: Easter egg?

-- | Main REPL loop using basic IO
loop :: REPL IO ()
loop = do
    current <- get
    liftIO $ writeIORef ref current -- Update global store for prompt export
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
            Write _ -> writeIMP
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

-- | Clear the terminal and display welcome message.
clear :: REPL IO ()
clear = liftIO js_writeWelcome

-- | Write trace to new browser tab as plaintext blob.
writeIMP :: REPL IO ()
writeIMP = do
    content <- gets (prettytrace . _trace)
    liftIO . js_writeIMP $ toJSString content
    throwError . Info $ "wrote trace to new tab"
