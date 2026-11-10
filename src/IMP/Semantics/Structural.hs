{- |
Module      : IMP.Semantics.Structural
Description : Structural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of structural (small-step) semantics for the IMP language.
Entirely pure: a step is a serialisable value between configurations, and
execution may block on external input, havoc (arbitrary integer) or a
nondeterministic choice, resuming once the corresponding resolution arrives.

The evaluation context of a suspended configuration is retained in the focus
'Gamma' between 'step' and 'resume'; both functions recurse over the same
statement contexts ('Seq', 'TryCatch', 'Timeout', 'Alternate'), so every
'Blocked' configuration produced has exactly one matching 'resume' path.

The internal 'Restore' and 'Return' statements carry state snapshots within
the residual statement, which keeps the machine stack-free: a machine is
nothing but a 'Gamma'.
-}
module IMP.Semantics.Structural (
    Suspension (..),
    Resolution (..),
    Effect (..),
    Rule (..),
    Step (..),
    Configuration (..),
    step,
    resume,
) where

import qualified Data.Map as Map

import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

-- | Blocking step awaiting an external decision or value.
data Suspension
    = AwaitInput String -- ^ @read@ statement awaits an integer for the variable.
    | AwaitHavoc String -- ^ @havoc@ statement awaits an arbitrary integer for the variable.
    | AwaitChoice -- ^ Nondeterministic or parallel statement awaits which side to take.
    deriving (Eq, Show)

-- | Unblocking step with nondeterministic choice or read value.
data Resolution
    = ResolveInput Integer -- ^ Resolve @read@ suspension with the integer.
    | ResolveHavoc Integer -- ^ Resolve @havoc@ suspension with the integer.
    | ResolveChoice Bool -- ^ Resolve choice suspension, whether to branch left.
    deriving (Eq, Show)

-- | Observable effect emitted by a step.
data Effect = Output String
    deriving (Eq, Show)

-- | Applied inference rule of a performed step.
{- FOURMOLU_DISABLE -}
data Rule
    = RuleVarDef -- ^ Variable definition.
    | RuleSeq -- ^ Sequential composition.
    | RuleIfThen -- ^ Conditional, taken branch.
    | RuleIfElse -- ^ Conditional, untaken branch.
    | RuleWhile -- ^ Loop unrolling.
    | RulePrint -- ^ Output of evaluation.
    | RuleRead -- ^ Input into variable.
    | RuleLocal -- ^ Local variable scope entry.
    | RuleChoice -- ^ Resolved nondeterministic or parallel choice.
    | RuleProcDef -- ^ Procedure definition.
    | RuleProcInvoc -- ^ Procedure invocation.
    | RuleRestore -- ^ State snapshot restoration.
    | RuleReturn -- ^ Procedure return to caller state.
    | RuleBreak -- ^ Break flag.
    | RuleRevert -- ^ Transactional execution.
    | RuleMatch -- ^ Pattern match.
    | RuleHavoc -- ^ Arbitrary variable definition.
    | RuleAssert -- ^ Assertion check.
    | RuleFlip -- ^ Alternating execution, flip side.
    | RuleFlop -- ^ Alternating execution, flop side.
    | RuleRaise -- ^ Raised exception.
    | RuleTryCatch -- ^ Exception handling.
    | RuleSwap -- ^ Variable swap.
    | RuleTimeout -- ^ Timeout counting.
    | RuleAlternate -- ^ Interleaved execution.
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

-- | One successfully performed step.
data Step = Step
    { residual :: Gamma -- ^ Configuration after the step.
    , effects :: [Effect] -- ^ Effects emitted by the step.
    , rule :: Rule -- ^ Applied inference rule.
    }
    deriving (Eq, Show)

-- | Machine configuration between steps.
data Configuration
    = Stepped Step -- ^ Step taken, residual configuration available.
    | Blocked Suspension -- ^ Execution suspended until externally resolved.
    | Stuck Exception -- ^ Execution failed.
    | Terminal State -- ^ Execution completed.
    deriving (Eq, Show)

-- | Small-step transition between configurations.
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
        in
            Stepped Step {residual = (Skip, state'), effects = [], rule = RuleVarDef}
    Seq s1 s2 ->
        case step (s1, state) of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleSeq}
            conf -> rewrap conf $ \(s1', state') -> (s1' <> s2, state')
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
    Local x a s ->
        let
            snapshot = ([(x, getVar state x)], Map.toList (getProcs state), getBreak state)
            local = setVar state x (evaluate a state)
        in
            Stepped Step {residual = (s <> Restore snapshot, local), effects = [], rule = RuleLocal}
    Par _ _ -> Blocked AwaitChoice
    NonDet _ _ -> Blocked AwaitChoice
    ProcDef name params rets body ->
        Stepped Step {residual = (Skip, setProc state name (params, rets, body)), effects = [], rule = RuleProcDef}
    ProcInvoc name arguments returns ->
        case getProc state name of
            Nothing -> Stuck . Error $ "undefined procedure: " ++ name
            Just (params, rets, body)
                | length arguments /= length params -> Stuck $ Error "mismatched number of arguments to parameters"
                | length returns /= length rets -> Stuck $ Error "mismatched number of return variables"
                | otherwise ->
                    let
                        vals = map (`evaluate` state) arguments -- evaluate arguments
                        local = (Map.fromList (zip params vals), getProcs state, getBreak state) -- into local state
                        snapshot = (Map.toList (getVars state), Map.toList (getProcs state), getBreak state)
                    in
                        Stepped $
                            Step
                                { residual = (body <> Return snapshot rets returns, local)
                                , effects = []
                                , rule = RuleProcInvoc
                                }
    Restore (vars, procs, flag) ->
        let state' = setVars (getVars state, Map.fromList procs, flag) vars
        in Stepped Step {residual = (Skip, state'), effects = [], rule = RuleRestore}
    Return (vars, procs, flag) rets returns ->
        let
            vals = map (getVar state) rets -- extract returns from callee state
            caller = (Map.fromList vars, Map.fromList procs, flag)
            state' = setVars caller $ zip returns vals -- insert into callside
        in
            Stepped Step {residual = (Skip, state'), effects = [], rule = RuleReturn}
    Break -> Stepped Step {residual = (Skip, setBreak state), effects = [], rule = RuleBreak}
    Revert s b ->
        -- INFO: Uninitialized variables can't be restored
        let snapshot = (Map.toList (getVars state), Map.toList (getProcs state), getBreak state)
        in Stepped Step {residual = (s <> IfElse b (Restore snapshot) Skip, state), effects = [], rule = RuleRevert}
    Match a ms d ->
        let v = evaluate a state
        in case lookup v ms of
            Just s -> Stepped Step {residual = (s, state), effects = [], rule = RuleMatch}
            Nothing -> Stepped Step {residual = (d, state), effects = [], rule = RuleMatch}
    Havoc x -> Blocked (AwaitHavoc x)
    Assert b ->
        if evaluate b state
            then Stepped Step {residual = (Skip, state), effects = [], rule = RuleAssert}
            else Stuck . AssertFail $ prettify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then Stepped Step {residual = (s1, setFlop state i), effects = [], rule = RuleFlop}
            else Stepped Step {residual = (s2, setFlip state i), effects = [], rule = RuleFlip}
    Raise a -> Stuck . Raised $ evaluate a state
    TryCatch s1 x s2 ->
        case step (s1, state) of
            Terminal state' -> Stepped Step {residual = (Skip, state'), effects = [], rule = RuleTryCatch}
            Stuck (Raised v) -> Stepped Step {residual = (s2, setVar state x v), effects = [], rule = RuleTryCatch}
            conf -> rewrap conf $ \(s1', state') -> (TryCatch s1' x s2, state')
    Swap x y ->
        let state' = setVars state [(x, getVar state y), (y, getVar state x)]
        in Stepped Step {residual = (Skip, state'), effects = [], rule = RuleSwap}
    Timeout s duration ->
        if evaluate duration state <= 0
            then Stepped Step {residual = (Skip, state), effects = [], rule = RuleTimeout}
            else
                case step (s, state) of
                    Terminal state' -> Stepped Step {residual = (Skip, state'), effects = [], rule = RuleTimeout}
                    conf -> rewrap conf $ \(s', state') -> (Timeout s' (duration - 1), state')
    Alternate s1 s2 ->
        case step (s1, state) of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleAlternate}
            conf -> rewrap conf $ \(s1', state') -> (Alternate s2 s1', state')

-- | Resume a suspended configuration with its resolution.
resume :: Gamma -> Resolution -> Configuration
resume (stm, state) resolution = case (stm, resolution) of
    (Read x, ResolveInput v) ->
        Stepped Step {residual = (Skip, setVar state x v), effects = [], rule = RuleRead}
    (Havoc x, ResolveHavoc v) ->
        Stepped Step {residual = (Skip, setVar state x v), effects = [], rule = RuleHavoc}
    (NonDet s1 s2, ResolveChoice left) ->
        Stepped Step {residual = (if left then s1 else s2, state), effects = [], rule = RuleChoice}
    (Par s1 s2, ResolveChoice left) ->
        Stepped Step {residual = (if left then s1 <> s2 else s2 <> s1, state), effects = [], rule = RuleChoice}
    (Seq s1 s2, _) ->
        case resume (s1, state) resolution of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleSeq}
            conf -> rewrap conf $ \(s1', state') -> (s1' <> s2, state')
    (TryCatch s1 x s2, _) ->
        case resume (s1, state) resolution of
            Terminal state' -> Stepped Step {residual = (Skip, state'), effects = [], rule = RuleTryCatch}
            Stuck (Raised v) -> Stepped Step {residual = (s2, setVar state x v), effects = [], rule = RuleTryCatch}
            conf -> rewrap conf $ \(s1', state') -> (TryCatch s1' x s2, state')
    (Timeout s duration, _) ->
        case resume (s, state) resolution of
            Terminal state' -> Stepped Step {residual = (Skip, state'), effects = [], rule = RuleTimeout}
            conf -> rewrap conf $ \(s', state') -> (Timeout s' (duration - 1), state')
    (Alternate s1 s2, _) ->
        case resume (s1, state) resolution of
            Terminal state' -> Stepped Step {residual = (s2, state'), effects = [], rule = RuleAlternate}
            conf -> rewrap conf $ \(s1', state') -> (Alternate s2 s1', state')
    (_, _) ->
        Stuck . Error $ "invalid resolution " ++ show resolution ++ " for statement: " ++ show stm

-- | Rewrap the residual of a stepped descent into the enclosing context, preserving effects and rule.
rewrap :: Configuration -> (Gamma -> Gamma) -> Configuration
rewrap conf f = case conf of
    Stepped inner -> Stepped inner {residual = f (residual inner)}
    suspended -> suspended
