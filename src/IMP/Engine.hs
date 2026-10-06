{- |
Module      : IMP.Engine
Description : Runtime session engine over the pure kernel API
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : experimental
Portability : portable

Session management layer over the pure kernel semantics.
Keeps current focus, history, and handles resumable execution.
-}
module IMP.Engine (
    Engine,
    Fault (..),
    Session (..),
    boot,
    snapshot,
    advance,
    resolve,
) where

import Control.Monad.Except
import Control.Monad.State

import IMP.Exception
import IMP.Kernel

-- | Engine monad stack over abstract base monad.
type Engine m = StateT Session (ExceptT Fault m)

-- | Engine-level failures.
data Fault
    = InvalidResolution Gamma Resolution
    | SessionBlocked Gamma Suspension
    | SessionStuck Exception
    deriving (Eq, Show)

-- | Session state managed by the engine.
data Session = Session
    { focus :: Gamma
    , current :: Configuration
    , history :: [Configuration]
    }
    deriving (Eq, Show)

-- | Initialize a session from an initial gamma.
boot :: Gamma -> Session
boot gamma =
    Session
        { focus = gamma
        , current = step gamma
        , history = []
        }

-- | Read current machine configuration.
snapshot :: (Monad m) => Engine m Configuration
snapshot = gets current

-- | Advance one deterministic step.
advance :: (Monad m) => Engine m Configuration
advance = do
    session <- get
    case current session of
        Stepped stepped -> do
            let gamma = residual stepped
            let next = step gamma
            put
                session
                    { focus = gamma
                    , current = next
                    , history = current session : history session
                    }
            return next
        Blocked suspension -> throwError $ SessionBlocked (focus session) suspension
        Stuck e -> throwError $ SessionStuck e
        Terminal _ -> return $ current session

-- | Resolve the current suspension and continue.
resolve :: (Monad m) => Resolution -> Engine m Configuration
resolve resolution = do
    session <- get
    case current session of
        Blocked _ -> do
            let next = IMP.Kernel.resume (focus session) resolution
            put
                session
                    { current = next
                    , history = current session : history session
                    , focus = case next of
                        Stepped stepped -> residual stepped
                        _ -> focus session
                    }
            return next
        _ -> throwError $ InvalidResolution (focus session) resolution
