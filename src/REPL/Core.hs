{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : REPL.Core
Description : Frontend-agnostic core of the IMP language REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

The REPL logic shared by all frontends. A frontend reads a line, possibly
continued over several lines while 'REPL.Input.incomplete', and passes it to
'submit', which interprets the metacommand or construct and returns the
updated session. Frontends provide 'MonadREPL' for their context. Failures
are reported and leave the session as it was before the input. Verbosity
@profile@ reports elapsed time and steps taken after each statement, @debug@
additionally displays the parsed statement and every structural step.
-}
module REPL.Core (
    MonadREPL (..),
    submit,
    program,
    welcome,
    goodbye,
    prompt,
    continuation,
    version,
) where

import Control.Monad (when)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.State.Strict (MonadState, StateT, lift, modify, runStateT)
import Data.Char (isSpace)
import Data.Version (showVersion)
import Text.Printf (printf)

import IMP.Check
import IMP.Effect
import IMP.Exception
import IMP.Expression
import IMP.Interpret
import IMP.Parser
import IMP.Pretty
import IMP.State (Flow (..), State, initial)
import IMP.Syntax
import REPL.Command
import REPL.Session

import qualified IMP.Semantics.Structural as Structural
import qualified Paths_impli as Paths

-- | Monad in which the REPL runs, providing terminal and file access.
class (MonadIMP m) => MonadREPL m where
    -- | Display line of REPL output, such as results, information and errors.
    say :: String -> m ()

    -- | Clear the screen.
    clear :: m ()

    -- | Read file content, or describe failure.
    load :: FilePath -> m (Either String String)

    -- | Write file content, or describe failure.
    save :: FilePath -> String -> m (Either String ())

    -- | Monotonic time in nanoseconds, for profiling.
    clock :: m Integer

-- | Message displayed on entry of the REPL.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands or :quit to exit."

-- | Message displayed on exit of the REPL.
goodbye :: String
goodbye = "Goodbye!"

-- | Prompt for input, separator marks verbosity level.
prompt :: Level -> String
prompt level = "IMP" ++ separator : " "
    where
        separator = case level of
            Normal -> '>'
            Profile -> '+'
            Debug -> '*'

-- | Prompt for continued input.
continuation :: String
continuation = "IMP| "

-- | Version of the interpreter.
version :: String
version = unwords ["impli", showVersion Paths.version]

-- | Interpret line of input in session, 'Nothing' to quit.
submit :: (MonadREPL m) => Session -> String -> m (Maybe Session)
submit session line = case dropWhile isSpace line of
    ":)" -> say "You look good today!" >> continue session -- it's true
    ':' : meta -> case parser "meta" meta of
        Left _ -> do
            say $ "unrecognized metacommand: :" ++ meta
            say "Enter :help to list available metacommands."
            continue session
        Right cmd -> command cmd session
    _ -> case parser @Construct "interactive" line of
        Left e -> report (ParseFail (show e)) >> continue session
        Right c -> construct c session >>= continue

-- | Interpret metacommand in session.
command :: (MonadREPL m) => Command -> Session -> m (Maybe Session)
command cmd session = case cmd of
    Help -> say helpText >> continue session
    Quit -> return Nothing
    Clear -> clear >> continue session
    Version -> say version >> continue session
    Reset aspect -> do
        inform $ case aspect of
            All -> "session reset"
            Vars -> "variables reset"
            Procs -> "procedures reset"
            Trace -> "trace reset"
        continue (reset aspect session)
    Show aspect -> say (render aspect session) >> continue session
    Load path ->
        load path >>= \case
            Left e -> report (IOFail (unlines ["read from: " ++ path, e])) >> continue session
            Right source -> case parser path source of
                Left e -> report (ParseFail (show e)) >> continue session
                Right stm ->
                    statement stm session >>= \case
                        Nothing -> continue session
                        Just session' -> inform ("interpreted: " ++ path) >> continue session'
    Write path ->
        save path (prettytrace (reverse (trace session)))
            >>= \case
                Left e -> report (IOFail (unlines ["write trace to: " ++ path, e]))
                Right () -> inform ("wrote trace to: " ++ path)
            >> continue session
    AST element -> do
        case element of
            Input c -> say (show c)
            Index n
                | n <= 0 || n > length (trace session) -> report . Error $ "index out of bounds: " ++ show n
                | otherwise -> say . show $ reverse (trace session) !! (n - 1)
        continue session
    Set (Interpretation sem) -> do
        inform $ "semantics: " ++ if sem == Natural then "natural" else "structural"
        continue session {semantics = sem}
    Set (Verbose level) -> do
        inform $ "verbosity: " ++ case level of
            Normal -> "normal"
            Profile -> "profile"
            Debug -> "debug"
        continue session {verbosity = level}

-- | Interpret construct in session.
construct :: (MonadREPL m) => Construct -> Session -> m Session
construct c session = case c of
    Statement stm -> maybe session id <$> statement stm session
    Arithmetic a -> either report (say . show) (evaluate a (state session)) >> return session
    Boolean b -> either report (say . boolean) (evaluate b (state session)) >> return session
    Whitespace -> return session
    where
        boolean v = if v then "true" else "false"

-- | Execute statement in session, recording it on success.
statement :: (MonadREPL m) => Stm -> Session -> m (Maybe Session)
statement stm session = case verbosity session of
    Normal ->
        attempt (execute (semantics session) stm (state session)) >>= \case
            Left e -> report e >> return Nothing
            Right state' -> return . Just $ record stm state' session
    level -> do
        when (level == Debug) $ say (show stm)
        start <- clock
        result <- attempt $ case (level, semantics session) of
            (Debug, Structural) -> stepwise stm (state session)
            (_, sem) -> counted (execute sem stm (state session))
        end <- clock
        case result of
            Left e -> report e >> return Nothing
            Right (state', steps) -> do
                say $ printf "[%s] elapsed: %.3f ms" (if level == Debug then "debug" else "profile") (millis (end - start))
                    ++ if semantics session == Structural then ", " ++ show steps ++ " steps" else ""
                return . Just $ record stm state' session
    where
        millis ns = fromInteger ns / 1e6 :: Double

-- | Interpretation counting ticks, the steps of structural semantics.
newtype Counted m a = Counted (StateT Integer m a)
    deriving (Applicative, Functor, Monad, MonadState Integer)

deriving instance (MonadError Exception m) => MonadError Exception (Counted m)

instance (MonadIMP m) => MonadIMP (Counted m) where
    input = Counted . lift . input
    output = Counted . lift . output
    choose = Counted (lift choose)
    arbitrary = Counted (lift arbitrary)
    tick = modify (+ 1) >> Counted (lift tick)

-- | Run interpretation, return result and number of ticks.
counted :: (MonadIMP m) => Counted m a -> m (a, Integer)
counted (Counted m) = runStateT m 0

-- | Execute statement with structural semantics, displaying every step.
stepwise :: (MonadREPL m) => Stm -> State -> m (State, Integer)
stepwise stm state' = liftEval (check stm) >> go 0 (Structural.step (Structural.Source stm, state'))
    where
        go !n conf = case conf of
            Structural.Stepped s -> do
                say $ drop 4 (show (Structural.rule s)) ++ " => " ++ stringify (fst (Structural.residual s))
                mapM_ (\(Structural.Output o) -> output o) (Structural.effects s)
                tick
                go (n + 1) (Structural.step (Structural.residual s))
            Structural.AwaitInput x k -> input x >>= go n . k
            Structural.AwaitHavoc _ k -> arbitrary >>= go n . k
            Structural.AwaitChoice k -> choose >>= go n . k
            Structural.Final (Next final) -> return (final, n)
            Structural.Final (Broken _) -> throwError $ Error "break outside loop" -- INFO: Unreachable after check
            Structural.Stuck e -> throwError e

-- | Parse and execute program in initial state, for non-interactive use.
program :: (MonadIMP m) => Semantics -> String -> String -> m (Either Exception State)
program sem channel source = case parser channel source of
    Left e -> return . Left $ ParseFail (show e)
    Right stm -> attempt (execute sem stm initial)

-- | Catch exception of interpretation.
attempt :: (MonadIMP m) => m a -> m (Either Exception a)
attempt action = (Right <$> action) `catchError` (return . Left)

-- | Display exception, aborted reads are silent.
report :: (MonadREPL m) => Exception -> m ()
report Empty = return ()
report e = say (show e)

-- | Display information.
inform :: (MonadREPL m) => String -> m ()
inform = say . show . Info

-- | Continue with session.
continue :: (Monad m) => Session -> m (Maybe Session)
continue = return . Just
