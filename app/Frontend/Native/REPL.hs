{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Frontend.Native.REPL
Description : Native Read-Evaluate-Print-Loop for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Native frontend of the REPL with haskeline line editing. Provides
'REPL.Core.MonadREPL' over haskeline and drives 'REPL.Core.submit'.
Interrupting with Ctrl-C aborts the current input or interpretation and
keeps the session.
-}
module Frontend.Native.REPL (
    Options (..),
    repl,
    interpret,
) where

import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isPrefixOf)
import System.Console.Haskeline
import System.Exit (exitFailure)
import System.Random (randomIO)
import Text.Read (readMaybe)

import qualified Control.Exception as Exception
import qualified System.Console.ANSI as ANSI

import IMP.Effect
import IMP.Exception
import IMP.Interpret
import REPL.Core
import REPL.Input
import REPL.Session

-- | Options of the native frontend.
data Options = Options
    { optSemantics :: Semantics
    -- ^ Semantics to interpret with.
    , optHistory :: Maybe FilePath
    -- ^ History file for haskeline.
    }

-- | Interpretation in haskeline.
newtype Native a = Native {unNative :: ExceptT Exception (InputT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError Exception)

-- | Run native interpretation.
runNative :: Native a -> InputT IO (Either Exception a)
runNative = runExceptT . unNative

-- | Lift haskeline action.
haskeline :: InputT IO a -> Native a
haskeline = Native . lift

instance MonadIMP Native where
    input x = do
        line <- haskeline $ getInputLine (x ++ " := ")
        case line of
            Nothing -> throwError Empty -- INFO: Ctrl-D aborts read
            Just answer -> case readMaybe answer of
                Just v -> return v
                Nothing -> say (show (Info "invalid input, please enter an integer")) >> input x
    output = haskeline . outputStrLn
    choose = liftIO randomIO
    arbitrary = liftIO randomIO

instance MonadREPL Native where
    say = haskeline . outputStrLn
    clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)
    load path = liftIO $ either (Left . show) Right <$> Exception.try @Exception.IOException (readFile' path)
        where
            readFile' p = readFile p >>= \s -> length s `seq` return s
    save path content = liftIO $ either (Left . show) Right <$> Exception.try @Exception.IOException (writeFile path content)

-- | Read-Evaluate-Print-Loop with haskeline for interactive use.
repl :: Options -> IO ()
repl opts = do
    putStrLn welcome
    runInputT settings . withInterrupt $ loop (fresh (optSemantics opts))
    putStrLn goodbye
    where
        settings = setComplete completer defaultSettings {historyFile = optHistory opts}
        completer = completeWord Nothing " \t();" $ \word ->
            return . map simpleCompletion $ completions (fresh (optSemantics opts)) word

-- | Loop reading and submitting input until quit or end of input.
loop :: Session -> InputT IO ()
loop session = do
    line <- handleInterrupt (return (Just "")) (readInput prompt "")
    case line of
        Nothing -> return ()
        Just input' -> do
            result <- handleInterrupt (interrupted >> return (Right (Just session))) (runNative (submit session input'))
            case result of
                Left e -> outputStrLn (show e) >> loop session
                Right Nothing -> return ()
                Right (Just session') -> loop session'
    where
        interrupted = outputStrLn (show Interrupted)

-- | Read possibly multi-line input, continuing while incomplete.
readInput :: String -> String -> InputT IO (Maybe String)
readInput label sofar = do
    line <- getInputLine label
    case line of
        Nothing
            | null sofar -> return Nothing
            | otherwise -> return (Just sofar) -- INFO: Ctrl-D submits what is there
        Just more -> do
            let input' = if null sofar then more else sofar ++ "\n" ++ more
            if incomplete input' && not (":" `isPrefixOf` input')
                then readInput continuation input'
                else return (Just input')

-- | Interpret IMP source non-interactively via channel name, exiting with failure on error.
interpret :: Semantics -> String -> String -> IO ()
interpret sem channel source = do
    result <- runInputT defaultSettings (runNative (program sem channel source))
    case result of
        Right (Right _) -> return ()
        Right (Left e) -> print e >> exitFailure
        Left e -> print e >> exitFailure
