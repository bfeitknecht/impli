{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad.Except (runExceptT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (CString, newCString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

import IMP.Expression
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax

type Env = ([Stm], State)

{-# NOINLINE global #-}
global :: IORef Env
global = unsafePerformIO $ newIORef ([], initial)

foreign export ccall impli :: CString -> IO CString

impli :: CString -> IO CString
impli c_in = do
    input <- peekCString c_in
    env <- readIORef global
    case parser "browser" input of
        Left e -> newCString $ "Parse error: " ++ show e
        Right c -> dispatch env c

dispatch :: Env -> Construct -> IO CString
dispatch (tr, st) cnstr = case cnstr of
    Statement stm ->
        runExceptT (interpret (stm, st))
            >>= either
                (\e -> newCString $ "Exception: " ++ show e)
                (\st' -> writeIORef global (stm : tr, st') >> newCString "OK")
    Arithmetic aexp ->
        let v = evaluate st aexp
        in newCString (show v)
    Boolean bexp ->
        let b = evaluate st bexp
        in newCString (if b then "true" else "false")
    Whitespace -> newCString ""

main :: IO ()
main = return ()
