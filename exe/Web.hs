{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)

import qualified GHC.Wasm.Prim as JS

import IMP.Expression
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax

-- | TODO
newtype Store = Store {reference :: IORef State}

-- | TODO
type Browser = StablePtr Store

foreign export javascript "initialize" initialize :: IO Browser
foreign export javascript "execute" execute :: Browser -> JS.JSString -> IO JS.JSString
foreign export javascript "release" release :: Browser -> IO ()

-- | Initialize the IMP language interpreter in the browser.
initialize :: IO Browser
initialize = newIORef initial >>= newStablePtr . Store

-- | TODO
execute :: Browser -> JS.JSString -> IO JS.JSString
execute ptr line = do
    Store ref <- deRefStablePtr ptr
    state <- readIORef ref
    let input = JS.fromJSString line
    case parser "browser" input of
        Left _ -> return $ JS.toJSString "{\"exception\": \"Not a valid Construct\"}"
        Right c ->
            runExceptT (dispatch state c)
                >>= either
                    (\e -> return . JS.toJSString $ "{\"exception\": \"" <> show e <> "\"}")
                    ( \state' -> do
                        writeIORef ref state'
                        return . JS.toJSString $ "{\"output\": \"ok\"}"
                    )

-- | TODO
release :: Browser -> IO ()
release = freeStablePtr

-- | TODO
dispatch :: State -> Construct -> IMP State
dispatch state cnstr = case cnstr of
    Statement stm -> interpret (stm, state) >>= return
    Arithmetic aexp -> (liftIO . print) (evaluate state aexp) >> return state
    Boolean bexp -> (liftIO . putStrLn) (if evaluate state bexp then "true" else "false") >> return state
    Whitespace -> return state

-- | Dummy stub to satisfy Cabal.
main :: IO ()
main = return ()
