{-# LANGUAGE FlexibleContexts #-}

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
replays observable behavior from the t'Derivation' context: the values that
@read@ consumed, the values @havoc@ produced and the branches
nondeterministic choice took during an earlier interactive execution, each in
chronological order. When a replay stream runs dry, deriving fails instead of
inventing values, so a derived tree is always identical to the execution it
replays. Everything printed accumulates in its @stdout@ field in order.
-}
module IMP.Semantics.Natural.Derive (
    Derivation (..),
    Rule (..),
    emptyDerivation,
    derivation,
    deriveWith,
    derive,
) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Data.List (isPrefixOf)
import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Replay context and accumulated effects of a derivation in progress.
data Derivation = Derivation
    { reads :: [Integer]
    -- ^ Values entered for @read@ during the recorded run, in order.
    , havocs :: [Integer]
    -- ^ Values produced by @havoc@ during the recorded run, in order.
    , choices :: [Bool]
    -- ^ Branches taken by nondeterministic choice during the recorded run.
    , stdout :: [String]
    -- ^ Accumulation of everything printed, in order.
    }
    deriving (Eq, Show)

-- | Derivation tree of the natural semantics for a statement.
data Rule = Rule
    { name :: String
    -- ^ Rule name as given in the paper.
    , conclusion :: String
    -- ^ Conclusion of the inference rule instance.
    , premise :: [Rule]
    -- ^ Premises, i.e. the derivations of the substatements.
    }
    deriving (Eq, Show)

-- | One concluded derivation: the applied rule and the resulting state.
data Node = Node {rule :: Rule, result :: State}

-- | Empty replay context.
emptyDerivation :: Derivation
emptyDerivation = Derivation {reads = [], havocs = [], choices = [], stdout = []}

-- | Replay context from the recorded resolutions of an execution.
derivation :: [Integer] -> [Integer] -> [Bool] -> Derivation
derivation reads' havocs' choices' = emptyDerivation {reads = reads', havocs = havocs', choices = choices'}

-- | Pretty-print the statement of a configuration.
source :: Stm -> String
source = prettify

-- | Visible bindings of a state, comma-separated.
bindings :: State -> String
bindings state = showBindings $ filter (not . ("_" `isPrefixOf`) . fst) $ Map.toList $ getVars state
    where
        showBindings [] = ""
        showBindings [(name, value)] = name ++ " := " ++ show value
        showBindings ((name, value) : rest) = name ++ " := " ++ show value ++ ", " ++ showBindings rest

-- | Conclusion of a rule instance: @< s, sigma > -> sigma'@.
to :: Gamma -> State -> String
to (stm, state) state' = "< " ++ source stm ++ ", {" ++ bindings state ++ "} > -> {" ++ bindings state' ++ "}"

-- | Elevate one derivation with the rule justifying it from its premises.
with :: Derivation -> String -> Gamma -> [Rule] -> State -> (Derivation, Node)
with derivation' label conf premises state' =
    (derivation', Node (Rule label (to conf state') premises) state')

-- | Leaf rule instance without premises.
leaf :: Derivation -> String -> Gamma -> State -> (Derivation, Node)
leaf derivation' label conf state' = with derivation' label conf [] state'

-- | Next value to give a @read@ statement: replay input, error when exhausted.
nextRead :: (MonadError Exception m) => Derivation -> String -> m (Integer, Derivation)
nextRead derivation' x = case reads derivation' of
    [] -> throwError . Error $ "derivation replay exhausted: no recorded input for read " ++ x
    (v : rest) -> return (v, derivation' {reads = rest})

-- | Next value to give a @havoc@ statement: replay value, error when exhausted.
nextHavoc :: (MonadError Exception m) => Derivation -> String -> m (Integer, Derivation)
nextHavoc derivation' x = case havocs derivation' of
    [] -> throwError . Error $ "derivation replay exhausted: no recorded value for havoc " ++ x
    (v : rest) -> return (v, derivation' {havocs = rest})

-- | Next side to take for a nondeterministic choice: replay, error when exhausted.
nextChoice :: (MonadError Exception m) => Derivation -> m (Bool, Derivation)
nextChoice derivation' = case choices derivation' of
    [] -> throwError . Error $ "derivation replay exhausted: no recorded nondeterministic choice"
    (left : rest) -> return (left, derivation' {choices = rest})

{- | Execute a statement like "IMP.Semantics.Natural" but return its
derivation tree alongside the final state; see the module documentation for
how @read@, @havoc@ and nondeterministic choice behave here.
-}
deriveWith :: (MonadError Exception m) => Derivation -> Gamma -> m (Derivation, Node)
deriveWith derivation' (stm, state) = case stm of
    Skip ->
        return $ leaf derivation' "Skip" (stm, state) state
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate a state
        in
            return $
                leaf derivation' "Assign" (stm, state) $
                    setVar state x $
                        case dop of
                            Def -> v'
                            Inc -> v + v'
                            Dec -> v - v'
                            Prod -> v * v'
                            Quot -> v // v'
                            Rem -> v %% v'
    Seq s1 s2 -> do
        (derivation1, left) <- deriveWith derivation' (s1, state)
        (derivation2, right) <- deriveWith derivation1 (s2, result left)
        return $ with derivation2 "Sequence" (stm, state) [rule left, rule right] (result right)
    IfElse b s1 s2 ->
        if evaluate b state
            then do
                (derivation1, node) <- deriveWith derivation' (s1, state)
                return $ with derivation1 "If" (stm, state) [rule node] (result node)
            else do
                (derivation1, node) <- deriveWith derivation' (s2, state)
                return $ with derivation1 "Else" (stm, state) [rule node] (result node)
    While b s ->
        if evaluate b state
            then
                if not $ getBreak state
                    then do
                        (derivation1, node) <- deriveWith derivation' (s <> While b s, state)
                        return $ with derivation1 "While" (stm, state) [rule node] (result node)
                    else return $ leaf derivation' "End" (stm, state) (resetBreak state)
            else return $ leaf derivation' "While" (stm, state) state
    Print e ->
        return
            ( derivation' {stdout = stdout derivation' ++ [show (evaluate e state)]}
            , Node (Rule "Print" (to (stm, state) state) []) state
            )
    Read x -> do
        (v, derivation1) <- nextRead derivation' x
        return $ leaf derivation1 "Read" (stm, state) (setVar state x v)
    Local x a s -> do
        let
            old = getVar state x
            new = evaluate a state
            local = setVar state x new
        (derivation1, node) <- deriveWith derivation' (s, local)
        let state' = setVar (result node) x old
        return (derivation1, Node (Rule "Local" (to (stm, state) state') [rule node]) state')
    Par s1 s2 -> do
        (derivation1, left) <- deriveWith derivation' (s1, state)
        (derivation2, right) <- deriveWith derivation1 (s2, state)
        let (vs1', ps1', flag') = result left
        let (vs2', ps2', _) = result right
        return $
            with derivation2 "Parallel" (stm, state) [rule left, rule right] $
                (Map.union vs1' vs2', Map.union ps1' ps2', flag')
    NonDet s1 s2 -> do
        (left, derivation1) <- nextChoice derivation'
        (derivation2, node) <- deriveWith derivation1 ((if left then s1 else s2), state)
        return $ with derivation2 "Nondeterminate" (stm, state) [rule node] (result node)
    ProcDef name params rets body ->
        return $ leaf derivation' "Procedure" (stm, state) $ setProc state name (params, rets, body)
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
                    (derivation1, node) <- deriveWith derivation' (body, local)
                    let
                        rets' = zip returns $ map (getVar (result node)) rets
                        state' = setVars state rets'
                    return (derivation1, Node (Rule "Invocation" (to (stm, state) state') [rule node]) state')
    Restore _ -> throwError . Error $ "illegal statement for derive in natural semantics: " ++ show stm
    Return {} -> throwError . Error $ "illegal statement for derive in natural semantics: " ++ show stm
    Break -> return $ leaf derivation' "Break" (stm, state) (setBreak state)
    Revert s b -> do
        (derivation1, node) <- deriveWith derivation' (s, state)
        if evaluate b (result node)
            then return (derivation1, Node (Rule "Revert" (to (stm, state) state) [rule node]) state)
            else return (derivation1, Node (Rule "Revert" (to (stm, state) (result node)) [rule node]) (result node))
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> do
                (derivation1, node) <- deriveWith derivation' (s, state)
                return $ with derivation1 "Case" (stm, state) [rule node] (result node)
            Nothing -> do
                (derivation1, node) <- deriveWith derivation' (d, state)
                return $ with derivation1 "Default" (stm, state) [rule node] (result node)
    Havoc x -> do
        (v, derivation1) <- nextHavoc derivation' x
        return $ leaf derivation1 "Havoc" (stm, state) (setVar state x v)
    Assert b ->
        if evaluate b state
            then return $ leaf derivation' "Pass" (stm, state) state
            else throwError . AssertFail $ prettify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then do
                (derivation1, node) <- deriveWith derivation' (s1, state)
                let state' = setFlop (result node) i
                return (derivation1, Node (Rule "Flip" (to (stm, state) state') [rule node]) state')
            else do
                (derivation1, node) <- deriveWith derivation' (s2, state)
                let state' = setFlip (result node) i
                return (derivation1, Node (Rule "Flop" (to (stm, state) state') [rule node]) state')
    Raise a -> throwError . Raised $ evaluate a state
    TryCatch s1 x s2 -> catchError (deriveWith derivation' (s1, state)) $ \e -> case e of
        Raised v -> do
            (derivation1, node) <- deriveWith derivation' (s2, setVar state x v)
            return (derivation1, Node (Rule "Catch" (to (stm, state) (result node)) [rule node]) (result node))
        _ -> throwError e
    Swap x y ->
        let
            v = getVar state x
            w = getVar state y
        in
            return $ leaf derivation' "Swap" (stm, state) $ setVars state [(x, w), (y, v)]
    Timeout _ _ -> throwError $ Error "timeout statement not supported in big-step semantics"
    Alternate _ _ -> throwError $ Error "alternate execution not supported in big-step semantics"

-- | Derive the statement in the given state, replaying the recording of an
-- earlier execution; fails when the recorded resolutions run out early.
derive :: (MonadError Exception m) => Derivation -> Gamma -> m (Derivation, Rule)
derive derivation' conf = do
    (derivation1, node) <- deriveWith derivation' conf
    return (derivation1, rule node)
