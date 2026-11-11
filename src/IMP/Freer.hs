{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : IMP.Freer
Description : Minimal freer monad machinery for algebraic effects
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Vendored replacement for the external @freer-simple@ package, which caps
@template-haskell < 2.22@ and therefore does not build with GHC 9.12.
Only the fragment of the API actually used by the kernel and engine is
implemented: 'Eff' with 'Member' constraints over an open 'Union', 'send',
'interpret', 'run', 'runM' and an 'Error' effect with 'throwError',
'catchError' and 'runError'. Semantics match
<https://hackage.haskell.org/package/freer-simple freer-simple>.
-}
module IMP.Freer (
    Eff,
    Member,
    LastMember,
    send,
    sendM,
    interpret,
    run,
    runM,
    Error,
    throwError,
    catchError,
    runError,
) where

-- | Open union of effects: a request of one of the effects in @effs@.
data Union effs a where
    Here :: eff a -> Union (eff ': effs) a
    There :: Union effs a -> Union (eff' ': effs) a

-- | Membership of an effect in a type-level list of effects.
class Member eff effs where
    -- | Inject an effect request into the union.
    inj :: eff a -> Union effs a

    -- | Project a request of the effect out of the union, if present.
    prj :: Union effs a -> Maybe (eff a)

instance Member eff (eff ': effs) where
    inj = Here
    prj (Here request) = Just request
    prj (There _) = Nothing

instance {-# OVERLAPPABLE #-} (Member eff effs) => Member eff (eff' ': effs) where
    inj = There . inj
    prj (Here _) = Nothing
    prj (There union) = prj union

-- | Membership in the final position of the effect row, disambiguated by list shape.
class (Member m effs) => LastMember m effs | effs -> m

instance {-# OVERLAPPABLE #-} (LastMember m effs) => LastMember m (eff ': effs)

instance LastMember m (m ': '[])

-- | Effectful computation over the effect row @effs@, a freer monad.
data Eff effs a
    = Pure a
    | forall x. Impure (Union effs x) (x -> Eff effs a)

instance Functor (Eff effs) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Impure union continue) = Impure union (fmap f . continue)

instance Applicative (Eff effs) where
    pure = Pure
    Pure f <*> action = fmap f action
    Impure union continue <*> action = Impure union ((<*> action) . continue)

instance Monad (Eff effs) where
    Pure x >>= k = k x
    Impure union continue >>= k = Impure union (\x -> continue x >>= k)

-- | Send an effect request to the surrounding effectful computation.
send :: (Member eff effs) => eff a -> Eff effs a
send request = Impure (inj request) Pure

-- | Send an action of the final monadic effect, aiding instance resolution.
sendM :: (LastMember m effs, Monad m) => m a -> Eff effs a
sendM = send

-- | Handle the topmost effect by translating its requests into the rest of the row.
interpret :: (forall x. eff x -> Eff effs x) -> Eff (eff ': effs) a -> Eff effs a
interpret _ (Pure x) = Pure x
interpret handle (Impure (Here request) continue) = handle request >>= interpret handle . continue
interpret handle (Impure (There union) continue) = Impure union (interpret handle . continue)

-- | Run a computation with no remaining effects, extracting the pure result.
run :: Eff '[] a -> a
run (Pure x) = x
run (Impure union _) = case union of {}

-- | Run a computation whose only remaining effect is the monad @m@ itself.
runM :: (Monad m) => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Impure (Here action) continue) = action >>= runM . continue
runM (Impure (There union) _) = case union of {}

-- | Exception effect carrying information of type @e@, no resumption.
data Error e a where
    Throw :: e -> Error e a

-- | Throw an error carrying information of type @e@.
throwError :: (Member (Error e) effs) => e -> Eff effs a
throwError = send . Throw

-- | Catch errors of type @e@; the handler may itself throw, such errors propagate.
catchError :: (Member (Error e) effs) => Eff effs a -> (e -> Eff effs a) -> Eff effs a
catchError (Pure x) _ = Pure x
catchError (Impure union continue) handler = case prj union of
    Just (Throw e) -> handler e
    Nothing -> Impure union (\x -> catchError (continue x) handler)

-- | Handle exception effects, communicating success or failure via 'Either'.
runError :: Eff (Error e ': effs) a -> Eff effs (Either e a)
runError (Pure x) = Pure (Right x)
runError (Impure (Here (Throw e)) _) = Pure (Left e)
runError (Impure (There union) continue) = Impure union (runError . continue)
