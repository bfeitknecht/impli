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
Sets up a custom stdin handle that bridges JavaScript input via JSFFI,
allowing the Haskell REPL to read from the browser terminal.
-}
module Main where

import REPL.Execute.Browser
import REPL.State

import Control.Monad (forever)
import GHC.IO.Handle (BufferMode (..), hDuplicateTo, hSetBuffering)
import GHC.Wasm.Prim (JSString, fromJSString)
import System.IO

-- | Export main entrypoint.
foreign export javascript "start" main :: IO ()

-- | Entrypoint for web/WASM IMP interpreter.
main :: IO ()
main = do
    -- h <- handler
    -- hSetBuffering h LineBuffering
    -- hDuplicateTo h stdin
    repl start -- INFO: Should never return
    error "how did we get here?"
