{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Main
Description : Web-Entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)

import qualified GHC.Wasm.Prim as JS

import IMP.Expression
import IMP.Parser
import IMP.Semantics.Structural
import IMP.State
import IMP.Syntax
import Web

-- | TODO
newtype Store = Store {reference :: IORef State}

-- | TODO
type Browser = StablePtr Store

-- | Foreign function to log JSString to the browser console.
foreign import javascript unsafe "console.log($1)" logger :: JS.JSString -> IO ()

foreign export javascript "initialize" initialize :: IO Browser
foreign export javascript "execute" execute :: Browser -> JS.JSString -> IO JS.JSString
foreign export javascript "release" release :: Browser -> IO ()
foreign export javascript "showTrace" showTrace :: IO JS.JSString

-- | Export trace source code to JS.
showTrace :: IO JS.JSString
showTrace = do
    Store ref <- deRefStablePtr ptr
    state <- readIORef ref
    -- (trace, _) <- readIORef ref
    -- return . JS.toJSString . prettify . mconcat $ trace
    return . JS.toJSString . prettify $ Skip

-- | Initialize the IMP language interpreter in the browser.
initialize :: IO Browser
initialize = newIORef initial >>= newStablePtr . Store

-- | TODO
execute :: Browser -> JS.JSString -> IO JS.JSString
execute ptr line = do
    Store ref <- deRefStablePtr ptr
    state <- readIORef ref
    let input = JS.fromJSString line
    -- console $ "Executing: " ++ input
    case parser "browser" input of
        Left _ -> do
            -- console $ "Parse error: Not a valid Construct"
            return $ JS.toJSString "{\"exception\": \"Not a valid Construct\"}"
        Right c ->
            runExceptT (dispatch' state c)
                >>= either
                    ( \e -> do
                        console $ "Execution error: " ++ show e
                        return . JS.toJSString $ "{\"exception\": \"" <> show e <> "\"}"
                    )
                    ( \state' -> do
                        console $ "Execution successful"
                        writeIORef ref state'
                        return . JS.toJSString $ "{\"output\": \"ok\"}"
                    )

-- | TODO
release :: Browser -> IO ()
release = freeStablePtr

-- | TODO
dispatch' :: State -> Construct -> IMP State
dispatch' state cnstr = case cnstr of
    Statement stm -> run (stm, state) >>= return -- interrupt after ~10s
    Arithmetic aexp -> (liftIO . print) (evaluate state aexp) >> return state
    Boolean bexp -> (liftIO . putStrLn) (if evaluate state bexp then "true" else "false") >> return state
    Whitespace -> return state

-- | console Haskell String to browser console.
console :: String -> IO ()
console = logger . JS.toJSString

-- | Dummy stub to satisfy Cabal.
main :: IO ()
main = return ()
