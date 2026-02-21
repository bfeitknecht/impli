{-# LANGUAGE JavaScriptFFI #-}

{- |
Module      : Main
Description : WASM entrypoint with JavaScript stdin bridging
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASM

Provides the main entrypoint for the web/WASM IMP interpreter.
Sets up a custom input action that bridges JavaScript input via JSFFI,
allowing the Haskell REPL to read from the browser terminal asynchronously.
-}
module Main where

import Data.IORef
import GHC.Wasm.Prim

import IMP.State (inputAction)
import REPL.Execute.Browser
import REPL.State

import System.IO

-- | Read input from JavaScript (awaits promise from @impli.readInput()@).
-- The 'safe' keyword is crucial here: it allows GHC's WASM backend to suspend
-- the Haskell thread and yield to the JS event loop until the Promise resolves.
foreign import javascript safe "await globalThis.impli.readInput()" js_readInput :: IO JSString

-- | Get line from terminal via JSFFI.
getInput :: IO String
getInput = fromJSString <$> js_readInput

-- | Export main entrypoint.
foreign export javascript "start" main :: IO ()

-- | Entrypoint for web/WASM IMP interpreter.
main :: IO ()
main = do
    -- Override the global input action to use our JSFFI bridge instead of standard getLine
    writeIORef inputAction getInput

    repl start -- INFO: Should never return
    error "how did we get here?"
