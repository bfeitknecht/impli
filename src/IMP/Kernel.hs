{- |
Module      : IMP.Kernel
Description : Pure kernel API for small-step and resumable execution
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : experimental
Portability : portable

Pure kernel layer exposing an API-centric stepping model.
Small-step execution is represented explicitly as values and can suspend on
external input and nondeterministic choice.
-}
module IMP.Kernel (
    Gamma,
    Suspension (..),
    Resolution (..),
    Effect (..),
    Rule (..),
    Step (..),
    Configuration (..),
    step,
    resume,
    run,
) where

import IMP.Exception
import IMP.Expression
import IMP.State
import IMP.Syntax

import qualified IMP.Semantics.Natural as Natural

-- | Tuple of current statement and state.
type Gamma = (Stm, State)

-- | Execution suspended until an external decision is provided.
data Suspension
    = AwaitChoice
    | AwaitInput String
    deriving (Eq, Show)

-- | External value resolving a suspension.
data Resolution
    = ResolveChoice Bool
    | ResolveInput Integer
    deriving (Eq, Show)

-- | Observable effect emitted by a step.
data Effect = Output String
    deriving (Eq, Show)

-- | Rule classification for a performed step.
data Rule
    = RuleSkip
    | RuleVarDef
    | RuleSeq
    | RuleIfThen
    | RuleIfElse
    | RuleWhile
    | RulePrint
    | RuleRead
    | RuleChoice
    | RuleBreak
    | RuleAssert
    | RuleSwap
    | RuleMatch
    deriving (Eq, Show)

-- | One successful step.
data Step = Step
    { residual :: Gamma
    , effects :: [Effect]
    , rule :: Rule
    }
    deriving (Eq, Show)

-- | Machine configuration between steps.
data Configuration
    = Stepped Step
    | Blocked Suspension
    | Stuck Exception
    | Terminal State
    deriving (Eq, Show)

-- | Small-step transition.
step :: Gamma -> Configuration
step (stm, state) = case stm of
    Skip -> Terminal state
    VarDef x dop a ->
        let
            v = getVar state x
            v' = evaluate a state
            state' = setVar state x $ case dop of
                Def -> v'
                Inc -> v + v'
                Dec -> v - v'
                Prod -> v * v'
                Quot -> v // v'
                Rem -> v %% v'
        in Stepped Step {residual = (Skip, state'), effects = [], rule = RuleVarDef}
    Seq s1 s2 ->
        case step (s1, state) of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleSeq}
            Stepped inner ->
                let
                    (s1', state') = residual inner
                in
                    Stepped inner {residual = (s1' <> s2, state')}
            Blocked suspension -> Blocked suspension
            Stuck e -> Stuck e
    IfElse b s1 s2 ->
        if evaluate b state
            then Stepped Step {residual = (s1, state), effects = [], rule = RuleIfThen}
            else Stepped Step {residual = (s2, state), effects = [], rule = RuleIfElse}
    While b s ->
        if evaluate b state
            then
                if getBreak state
                    then Stepped Step {residual = (Skip, resetBreak state), effects = [], rule = RuleWhile}
                    else Stepped Step {residual = (s <> While b s, state), effects = [], rule = RuleWhile}
            else Stepped Step {residual = (Skip, state), effects = [], rule = RuleWhile}
    Print a ->
        let printed = show (evaluate a state)
        in Stepped Step {residual = (Skip, state), effects = [Output printed], rule = RulePrint}
    Read x -> Blocked (AwaitInput x)
    Par _ _ -> Blocked AwaitChoice
    NonDet _ _ -> Blocked AwaitChoice
    Break -> Stepped Step {residual = (Skip, setBreak state), effects = [], rule = RuleBreak}
    Assert b ->
        if evaluate b state
            then Stepped Step {residual = (Skip, state), effects = [], rule = RuleAssert}
            else Stuck . AssertFail $ show b
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> Stepped Step {residual = (s, state), effects = [], rule = RuleMatch}
            Nothing -> Stepped Step {residual = (d, state), effects = [], rule = RuleMatch}
    Swap x y ->
        let
            vx = getVar state x
            vy = getVar state y
            state' = setVars state [(x, vy), (y, vx)]
        in Stepped Step {residual = (Skip, state'), effects = [], rule = RuleSwap}
    _ ->
        Stuck . Error $
            "kernel step currently unsupported for statement: " ++ show stm

-- | Resume a suspended configuration.
resume :: Gamma -> Resolution -> Configuration
resume (stm, state) resolution = case (stm, resolution) of
    (Read x, ResolveInput v) ->
        let state' = setVar state x v
        in Stepped Step {residual = (Skip, state'), effects = [], rule = RuleRead}
    (Seq s1 s2, r) ->
        case resume (s1, state) r of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleSeq}
            Stepped inner ->
                let
                    (s1', state') = residual inner
                in
                    Stepped inner {residual = (s1' <> s2, state')}
            Blocked suspension -> Blocked suspension
            Stuck e -> Stuck e
    (NonDet s1 s2, ResolveChoice left) ->
        Stepped Step {residual = (if left then s1 else s2, state), effects = [], rule = RuleChoice}
    (Par s1 s2, ResolveChoice left) ->
        Stepped Step {residual = (if left then s1 <> s2 else s2 <> s1, state), effects = [], rule = RuleChoice}
    (_, _) ->
        Stuck . Error $
            "invalid resolution " ++ show resolution ++ " for statement: " ++ show stm

-- | Execute with natural semantics to completion.
run :: Gamma -> IMP State
run = Natural.run
