{- |
Module      : Main
Description : WASM entrypoint for decoupled streaming REPL runtime
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASI

Provides the WASM entrypoint for the IMP interpreter using stdin/stdout
message streams. Input and asynchronous interrupt requests are routed through
an input broker thread so hosts can provide frontend-driven I/O.
-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Data.IORef (writeIORef)
import System.IO

import IMP.State (inputter, requestInterrupt)
import REPL.Execute.Browser
import REPL.State

-- | Recognized host messages that request an async interrupt.
isInterruptMessage :: String -> Bool
isInterruptMessage msg = msg == "\ETX" || msg == ":interrupt"

-- | Configure REPL input action backed by a queue and start stdin router.
configureInputRouter :: IO ()
configureInputRouter = do
    queue <- newTQueueIO
    writeIORef inputter $ \prompt -> do
        putStr prompt
        hFlush stdout
        atomically (readTQueue queue)
    _ <- forkIO $ router queue
    return ()
  where
    router queue = do
        result <- try getLine :: IO (Either IOException String)
        case result of
            Left _ -> atomically $ writeTQueue queue "\EOT"
            Right line ->
                do
                    if isInterruptMessage line
                        then requestInterrupt
                        else atomically (writeTQueue queue line)
                    router queue

-- | Entrypoint for the WASM REPL runtime.
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    configureInputRouter
    repl start
