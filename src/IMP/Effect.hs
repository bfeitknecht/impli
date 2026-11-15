{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : IMP.Effect
Description : Effects of interpretation for the IMP language
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

The interface through which the semantics reach the outside world. Both
"IMP.Semantics.Natural" and "IMP.Semantics.Structural" are written once over
'MonadIMP', frontends provide the instance for their context (haskeline,
browser, scripted tests). Failures are thrown as 'IMP.Exception.Exception'.
-}
module IMP.Effect (
    MonadIMP (..),
    liftEval,
) where

import Control.Monad.Except (MonadError, liftEither)

import IMP.Exception

-- | Monad in which IMP statements are interpreted.
class (MonadError Exception m) => MonadIMP m where
    -- | Read integer for variable, from @read x@.
    input :: String -> m Integer

    -- | Emit line of output, from @print a@.
    output :: String -> m ()

    -- | Decide nondeterministic or parallel branch, 'True' for left.
    choose :: m Bool

    -- | Produce arbitrary integer, from @havoc x@.
    arbitrary :: m Integer

    -- | Mark progress of one step or loop iteration, the point to yield or
    -- interrupt long-running interpretation by throwing 'Interrupted'.
    tick :: m ()
    tick = return ()

-- | Lift result of expression evaluation.
liftEval :: (MonadError Exception m) => Either Exception a -> m a
liftEval = liftEither
