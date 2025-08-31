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

-- | Foreign function to log JSString to the browser console.
foreign import javascript unsafe "console.log($1)" logger :: JS.JSString -> IO ()

-- | TODO
newtype Store = Store {reference :: IORef State}

-- | TODO
type Browser = StablePtr Store

foreign export javascript "initialize" initialize :: IO Browser
foreign export javascript "interpret" interpret :: Browser -> JS.JSString -> IO JS.JSString
foreign export javascript "release" release :: Browser -> IO ()

-- | Initialize the IMP language interpreter in the browser.
initialize :: IO Browser
initialize = newIORef initial >>= newStablePtr . Store

-- | TODO
interpret :: Browser -> JS.JSString -> IO JS.JSString
interpret ptr line = do
    Store ref <- deRefStablePtr ptr
    state <- readIORef ref
    let input = JS.fromJSString line
    console $ "Executing: " ++ input
    case parser "browser" input of
        Left _ -> do
            console $ "Parse error: Not a valid Construct"
            return $ JS.toJSString "{\"exception\": \"Not a valid Construct\"}"
        Right c ->
            runExceptT (dispatch state c)
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
dispatch :: State -> Construct -> IMP State
dispatch state cnstr = case cnstr of
    Statement stm -> execute (stm, state) >>= return
    Arithmetic aexp -> (liftIO . print) (evaluate state aexp) >> return state
    Boolean bexp -> (liftIO . putStrLn) (if evaluate state bexp then "true" else "false") >> return state
    Whitespace -> return state

-- transform :: Either Exception State -> JS.JSVal
-- transform res = either (\e -> undefined) (\s -> undefined) res

-- | console Haskell String to browser console.
console :: String -> IO ()
console = logger . JS.toJSString

-- | Dummy stub to satisfy Cabal.
main :: IO ()
main = return ()
