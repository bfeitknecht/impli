{-# LANGUAGE JavaScriptFFI #-}

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

import REPL.Execute.Browser
import REPL.State

import GHC.Wasm.Prim

-- | Read input from JavaScript (awaits promise from @impli.readIn()@)
foreign import javascript safe "await globalThis.impli.readInput()" js_readInput :: IO JSString

-- | Get line from terminal via JSFFI
getInput :: IO String
getInput = fromJSString <$> js_readInput

foreign export javascript "start" main :: IO ()

-- | Entrypoint for web/WASM IMP interpreter
main :: IO ()
main = repl start >> error "how did we get here?"
