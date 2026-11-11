{-# LANGUAGE RankNTypes #-}

{- |
Module      : IMP.Handlers
Description : Handlers record for discharging interpreter-world interaction
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Frontends supply these handlers so the natural semantics and the engine can
reach the outside world without the kernel knowing about that world. The
natural semantics discharges input, output, choice and havoc through them;
the engine additionally polls for interrupts while stepping the structural
semantics.
-}
module IMP.Handlers (Handlers (..), hoistHandlers) where

-- | Handlers supplied by frontends to discharge kernel suspensions and effects.
data Handlers m = Handlers
    { onInput :: String -> m Integer
    -- ^ Read value for variable.
    , onOutput :: String -> m ()
    -- ^ Emit output line.
    , onChoice :: m Bool
    -- ^ Decide nondeterministic or parallel branch.
    , onRandom :: m Integer
    -- ^ Produce arbitrary integer.
    , pollInterrupt :: m Bool
    -- ^ Whether an interrupt is pending.
    }

-- | Lift handlers to an ambient monad (e.g. 'ExceptT' over the frontend's).
hoistHandlers :: (forall a. m a -> n a) -> Handlers m -> Handlers n
hoistHandlers hoist (Handlers input output choice random interrupt) =
    Handlers
        { onInput = hoist . input
        , onOutput = hoist . output
        , onChoice = hoist choice
        , onRandom = hoist random
        , pollInterrupt = hoist interrupt
        }
