{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
are reported and leave the session as it was before the input.
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

import Control.Monad.Except (catchError)
import Data.Char (isSpace)
import Data.Version (showVersion)

import IMP.Effect
import IMP.Exception
import IMP.Expression
import IMP.Interpret
import IMP.Parser
import IMP.Pretty
import IMP.State (State, initial)
import IMP.Syntax
import REPL.Command
import REPL.Session

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

-- | Message displayed on entry of the REPL.
welcome :: String
welcome = "Welcome to the IMP REPL! Enter :help to list available metacommands or :quit to exit."

-- | Message displayed on exit of the REPL.
goodbye :: String
goodbye = "Goodbye!"

-- | Prompt for input.
prompt :: String
prompt = "IMP> "

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
statement stm session =
    attempt (execute (semantics session) stm (state session)) >>= \case
        Left e -> report e >> return Nothing
        Right state' -> return . Just $ record stm state' session

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
