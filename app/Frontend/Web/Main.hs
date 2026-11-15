{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Main
Description : Browser frontend of the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : wasm32

WASM reactor running in a web worker. The session lives in this instance
and the host submits lines of input through the exports. Output is streamed
to the host, @read@ awaits the host, and long-running interpretation yields
to the worker event loop regularly, where an interrupt can be requested.
Integers only cross the boundary as strings.
-}
module Main where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import GHC.Wasm.Prim
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import Text.Read (readMaybe)

import IMP.Effect
import IMP.Exception
import IMP.Interpret
import REPL.Core hiding (submit)
import REPL.Input (completions)
import REPL.Session

import qualified REPL.Core as Core
import qualified REPL.Input as Input

foreign import javascript unsafe "globalThis.impli.output($1)"
    js_output :: JSString -> IO ()

foreign import javascript unsafe "globalThis.impli.clear()"
    js_clear :: IO ()

foreign import javascript safe "await globalThis.impli.input($1)"
    js_input :: JSString -> IO JSString

foreign import javascript safe "await globalThis.impli.pause()"
    js_pause :: IO Int

foreign import javascript unsafe "globalThis.impli.exists($1)"
    js_exists :: JSString -> IO Int

foreign import javascript unsafe "globalThis.impli.load($1)"
    js_load :: JSString -> IO JSString

foreign import javascript unsafe "globalThis.impli.save($1, $2)"
    js_save :: JSString -> JSString -> IO ()

-- | Session of this instance.
session :: IORef Session
session = unsafePerformIO $ newIORef (fresh Natural)
{-# NOINLINE session #-}

-- | Ticks since last yield to the host.
ticks :: IORef Int
ticks = unsafePerformIO $ newIORef 0
{-# NOINLINE ticks #-}

-- | Number of ticks between yields to the host.
quantum :: Int
quantum = 20000

-- | Interpretation in the browser.
newtype Web a = Web {unWeb :: ExceptT Exception IO a}
    deriving (Applicative, Functor, Monad, MonadError Exception, MonadIO)

instance MonadIMP Web where
    input x = do
        answer <- liftIO $ fromJSString <$> js_input (toJSString (x ++ " := "))
        case answer of
            "\EOT" -> throwError Empty -- INFO: Ctrl-D aborts read
            "\ETX" -> throwError Interrupted
            _ -> case readMaybe answer of
                Just v -> return v
                Nothing -> say (show (Info "invalid input, please enter an integer")) >> input x
    output = liftIO . js_output . toJSString
    choose = liftIO randomIO
    arbitrary = liftIO randomIO
    tick = do
        n <- liftIO $ readIORef ticks
        if n < quantum
            then liftIO $ writeIORef ticks (n + 1)
            else do
                liftIO $ writeIORef ticks 0
                interrupted <- liftIO js_pause
                if interrupted /= 0 then throwError Interrupted else return ()

instance MonadREPL Web where
    say = output
    clear = liftIO js_clear
    load path = liftIO $ do
        found <- js_exists (toJSString path)
        if found /= 0
            then Right . fromJSString <$> js_load (toJSString path)
            else return (Left "no such example, try :load followed by tab")
    save path content = liftIO $ Right <$> js_save (toJSString path) (toJSString content)

-- | Reset to a fresh session and greet.
boot :: IO ()
boot = do
    writeIORef session (fresh Natural)
    js_output (toJSString welcome)

-- | Submit line of input, 1 if the REPL quit and restarted, else 0.
submit :: JSString -> IO Int
submit line = do
    current <- readIORef session
    writeIORef ticks 0
    result <- runExceptT . unWeb $ submit' current
    case result of
        Right (Just next) -> writeIORef session next >> return 0
        Right Nothing -> js_output (toJSString goodbye) >> boot >> return 1
        Left e -> js_output (toJSString (show e)) >> return 0
    where
        submit' current = Core.submit current (fromJSString line)

-- | Completion candidates for word prefix, newline separated.
complete :: JSString -> IO JSString
complete prefix = do
    current <- readIORef session
    return . toJSString . unlines $ completions current (fromJSString prefix)

-- | Whether input continues on the next line, 1 if so.
incomplete :: JSString -> IO Int
incomplete line = return $ if Input.incomplete (fromJSString line) then 1 else 0

foreign export javascript "boot" boot :: IO ()
foreign export javascript "submit" submit :: JSString -> IO Int
foreign export javascript "complete" complete :: JSString -> IO JSString
foreign export javascript "incomplete" incomplete :: JSString -> IO Int

-- | Unused, the reactor is driven through its exports.
main :: IO ()
main = return ()
