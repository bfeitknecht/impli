{-# LANGUAGE JavaScriptFFI #-}

{- |
Module      : Main
Description : WASM API entrypoint for JavaScript-driven REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASM

Exposes an API-centric WASM interface where JavaScript drives prompt/input and
submits lines for evaluation.
-}
module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Data.IORef
import GHC.Wasm.Prim
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import IMP.State (inputter)
import REPL.Execute.Browser
import REPL.State

-- | Read input from JavaScript callback.
foreign import javascript safe "await globalThis.impliReadInput($1)" js_readInput :: JSString -> IO JSString

-- | Session state shared across API calls.
{-# NOINLINE storeRef #-}
storeRef :: IORef Store
storeRef = unsafePerformIO (newIORef start)

-- | Get line from terminal via JS callback.
getInput :: String -> IO String
getInput prompt = fromJSString <$> js_readInput (toJSString prompt)

-- | Export startup API.
foreign export javascript "start" startAPI :: IO ()

-- | Export line evaluation API.
foreign export javascript "evalLine" evalLineAPI :: JSString -> IO Bool

-- | Export prompt query API.
foreign export javascript "getPrompt" getPromptAPI :: IO JSString

-- | Initialize runtime state for browser use.
startAPI :: IO ()
startAPI = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    writeIORef inputter getInput
    writeIORef storeRef start

-- | Evaluate one line in the current session.
evalLineAPI :: JSString -> IO Bool
evalLineAPI jsInput = do
    store <- readIORef storeRef
    result <- runExceptT $ runStateT (runLine (fromJSString jsInput)) store
    case result of
        Left e -> print e >> return True
        Right (continue, store') -> do
            writeIORef storeRef store'
            return continue

-- | Current prompt including separator and trailing space.
getPromptAPI :: IO JSString
getPromptAPI = do
    store <- readIORef storeRef
    return . toJSString $ _prompt store ++ _separator store : " "
