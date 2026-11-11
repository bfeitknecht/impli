{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IMP.Semantics.Natural.Derive
Description : Derivation trees of the natural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Derivation-tree rendering of the natural (big-step) semantics. Execution
follows the same rules as "IMP.Semantics.Natural", but instead of only the
final state it returns the inference tree that justifies it, with rule names
and configurations matching the notation of the paper.

Because plain 'IMP.Semantics.Natural.run' must not be overloaded, 'derive'
commits to observable behavior for interactive statements: @read@ consumes
the replay list supplied via the t'Derivation' (intended to be fed with the
values entered during an earlier interactive run), while @havoc@ and
nondeterministic choice are decided by the seeded generator in the
t'Derivation'. Everything printed is appended to its @stdout@ accumulator in
the order it would occur, so deriving a program replays its execution.
-}
module IMP.Semantics.Natural.Derive (
    Derivation (..),
    Rule (..),
    deriveWith,
    derive,
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Data.Aeson (ToJSON)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Replay context and accumulated effects of a derivation in progress.
data Derivation = Derivation
    { seed :: Integer
    -- ^ Feeds nondeterministic choices and @havoc@ values.
    , stdin :: [Integer]
    -- ^ Replay of the values entered for @read@, in order.
    , stdout :: [String]
    -- ^ Accumulation of everything printed, in order.
    }
    deriving (Eq, Show, Generic)

instance ToJSON Derivation

-- | Derivation tree of the natural semantics for a statement.
data Rule = Rule
    { name :: String
    -- ^ Rule name as given in the paper.
    , conclusion :: String
    -- ^ Conclusion of the inference rule instance.
    , premise :: [Rule]
    -- ^ Premises, i.e. the derivations of the substatements.
    }
    deriving (Eq, Show, Generic)

instance ToJSON Rule

-- | One concluded derivation: the applied rule and the resulting state.
data Node = Node {rule :: Rule, result :: State}

-- | Pretty-print the statement of a configuration.
source :: Stm -> String
source = prettify

-- | Visible bindings of a state, comma-separated.
bindings :: State -> String
bindings state = showBindings $ filter (not . ("_" `isPrefixOf`) . fst) $ getVars state
    where
        showBindings [] = ""
        showBindings [(name, value)] = name ++ " := " ++ show value
        showBindings ((name, value) : rest) = name ++ " := " ++ show value ++ ", " ++ showBindings rest

-- | Conclusion of a rule instance: @< s, sigma > -> sigma'@.
to :: Gamma -> State -> String
to (stm, state) state' = "< " ++ source stm ++ ", {" ++ bindings state ++ "} > -> {" ++ bindings state' ++ "}"

-- | Elevate one derivation with the rule justifying it from its premises.
with :: Derivation -> String -> Gamma -> [Rule] -> State -> (Derivation, Node)
with derivation label conf premises state' =
    (derivation, Node (Rule label (to conf state') premises) state')

-- | Leaf rule instance without premises.
leaf :: Derivation -> String -> Gamma -> State -> (Derivation, Node)
leaf derivation label conf state' = with derivation label conf [] state'

-- | Seeded pseudo-random value (linear congruential generator).
random :: Integer -> Integer
random value = value * 6364136223846793005 + 1442695040888963407

-- | Draw the next non-negative pseudo-random value.
draw :: Derivation -> (Integer, Derivation)
draw derivation =
    let value = random (seed derivation)
    in (abs value `mod` 2147483647, derivation {seed = value})

-- | Next value to give a @read@ statement: replay input, zero when exhausted.
next :: Derivation -> (Integer, Derivation)
next derivation = case stdin derivation of
    [] -> (0, derivation)
    (v : rest) -> (v, derivation {stdin = rest})

{- | Execute a statement like "IMP.Semantics.Natural" but return its
derivation tree alongside the final state; see the module documentation for
how @read@, @havoc@ and nondeterministic choice behave here.
-}
deriveWith :: (MonadError Exception m) => Derivation -> Gamma -> m (Derivation, Node)
deriveWith derivation (stm, state) = case stm of
    Skip ->
        return $ leaf derivation "Skip" (stm, state) state
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate a state
        in
            return $
                leaf derivation "Assign" (stm, state) $
                    setVar state x $
                        case dop of
                            Def -> v'
                            Inc -> v + v'
                            Dec -> v - v'
                            Prod -> v * v'
                            Quot -> v // v'
                            Rem -> v %% v'
    Seq s1 s2 -> do
        (derivation', left) <- deriveWith derivation (s1, state)
        (derivation'', right) <- deriveWith derivation' (s2, result left)
        return $ with derivation'' "Sequence" (stm, state) [rule left, rule right] (result right)
    IfElse b s1 s2 ->
        if evaluate b state
            then do
                (derivation', node) <- deriveWith derivation (s1, state)
                return $ with derivation' "If" (stm, state) [rule node] (result node)
            else do
                (derivation', node) <- deriveWith derivation (s2, state)
                return $ with derivation' "Else" (stm, state) [rule node] (result node)
    While b s ->
        if evaluate b state
            then
                if not $ getBreak state
                    then do
                        (derivation', node) <- deriveWith derivation (s <> While b s, state)
                        return $ with derivation' "While" (stm, state) [rule node] (result node)
                    else return $ leaf derivation "End" (stm, state) (resetBreak state)
            else return $ leaf derivation "While" (stm, state) state
    Print e ->
        return
            ( derivation {stdout = stdout derivation ++ [show (evaluate e state)]}
            , Node (Rule "Print" (to (stm, state) state) []) state
            )
    Read x -> do
        let (v, derivation') = next derivation
        return $ leaf derivation' "Read" (stm, state) (setVar state x v)
    Local x a s -> do
        let
            old = getVar state x
            new = evaluate a state
            local = setVar state x new
        (derivation', node) <- deriveWith derivation (s, local)
        let state' = setVar (result node) x old
        return (derivation', Node (Rule "Local" (to (stm, state) state') [rule node]) state')
    Par s1 s2 -> do
        (derivation', left) <- deriveWith derivation (s1, state)
        (derivation'', right) <- deriveWith derivation' (s2, state)
        let (vs1', ps1', flag') = result left
        let (vs2', ps2', _) = result right
        return $
            with derivation'' "Parallel" (stm, state) [rule left, rule right] $
                (Map.union vs1' vs2', Map.union ps1' ps2', flag')
    NonDet s1 s2 -> do
        let (v, derivation') = draw derivation
        let taken = if even v then s1 else s2
        (derivation'', node) <- deriveWith derivation' (taken, state)
        return $ with derivation'' "Nondeterminate" (stm, state) [rule node] (result node)
    ProcDef name parameters returns body ->
        return $ leaf derivation "Procedure" (stm, state) $ setProc state name (parameters, returns, body)
    ProcInvoc name arguments returns ->
        case getProc state name of
            Nothing -> throwError . Error $ "undefined procedure: " ++ name
            Just (params, rets, body)
                | length arguments /= length params -> throwError $ Error "mismatched number of arguments to parameters"
                | length returns /= length rets -> throwError $ Error "mismatched number of return variables"
                | otherwise -> do
                    let
                        vals = map (`evaluate` state) arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state)
                    (derivation', node) <- deriveWith derivation (body, local)
                    let
                        rets' = zip returns $ map (getVar (result node)) rets
                        state' = setVars state rets'
                    return (derivation', Node (Rule "Invocation" (to (stm, state) state') [rule node]) state')
    Restore _ -> throwError . Error $ "illegal statement for derive in natural semantics: " ++ show stm
    Return {} -> throwError . Error $ "illegal statement for derive in natural semantics: " ++ show stm
    Break -> return $ leaf derivation "Break" (stm, state) (setBreak state)
    Revert s b -> do
        (derivation', node) <- deriveWith derivation (s, state)
        if evaluate b (result node)
            then return (derivation', Node (Rule "Revert" (to (stm, state) state) [rule node]) state)
            else return (derivation', Node (Rule "Revert" (to (stm, state) (result node)) [rule node]) (result node))
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> do
                (derivation', node) <- deriveWith derivation (s, state)
                return $ with derivation' "Case" (stm, state) [rule node] (result node)
            Nothing -> do
                (derivation', node) <- deriveWith derivation (d, state)
                return $ with derivation' "Default" (stm, state) [rule node] (result node)
    Havoc x -> do
        let (v, derivation') = draw derivation
        return $ leaf derivation' "Havoc" (stm, state) (setVar state x v)
    Assert b ->
        if evaluate b state
            then return $ leaf derivation "Pass" (stm, state) state
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then do
                (derivation', node) <- deriveWith derivation (s1, state)
                return (derivation', Node (Rule "Flip" (to (stm, state) (setFlop (result node) i)) [rule node]) (setFlop (result node) i))
            else do
                (derivation', node) <- deriveWith derivation (s2, state)
                return (derivation', Node (Rule "Flop" (to (stm, state) (setFlip (result node) i)) [rule node]) (setFlip (result node) i))
    Raise a -> throwError . Raised $ evaluate a state
    TryCatch s1 x s2 -> catchError (deriveWith derivation (s1, state)) $ \e -> case e of
        Raised v -> do
            (derivation', node) <- deriveWith derivation (s2, setVar state x v)
            return (derivation', Node (Rule "Catch" (to (stm, state) (result node)) [rule node]) (result node))
        _ -> throwError e
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return $ leaf derivation "Swap" (stm, state) $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError $ Error "timeout statement not supported in big-step semantics"
    Alternate _ _ -> throwError $ Error "alternate execution not supported in big-step semantics"

{- | Derive the statement in the given state without any input replay,
seeding the nondeterminism from the derivation context.
-}
derive :: (MonadError Exception m) => Derivation -> Gamma -> m (Derivation, Rule)
derive derivation conf = do
    (derivation', node) <- deriveWith derivation conf
    return (derivation', rule node)
