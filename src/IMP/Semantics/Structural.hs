{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : IMP.Semantics.Structural
Description : Structural semantics for the IMP language
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Implementation of structural (small-step) semantics for the IMP language.

A configuration is a 'Term' and a 'State'. Terms extend source statements
with the runtime contexts that arise during execution (sequential
continuation, loop iteration, local scope, procedure call frame, ...), which
keeps the source syntax free of internal constructs.

'step' is pure: a single transition either completes with a 'Step', blocks
on input, havoc or choice with a continuation awaiting the value, or ends
with a final control flow outcome or exception. 'drive' discharges blocked
configurations through 'IMP.Effect.MonadIMP' and runs to completion.
-}
module IMP.Semantics.Structural (
    Term (..),
    Gamma,
    Effect (..),
    Rule (..),
    Step (..),
    Configuration (..),
    step,
    drive,
    run,
    measure,
) where

import Control.Monad.Except (throwError)
import Prettyprinter

import IMP.Effect
import IMP.Exception
import IMP.Expression
import IMP.Pretty
import IMP.State
import IMP.Syntax

import qualified Data.Map.Strict as Map

{- FOURMOLU_DISABLE -}
-- | Statement under execution, source statement in runtime context.
data Term
    = Source Stm -- ^ Statement yet to execute.
    | Then Term Stm -- ^ Sequential continuation.
    | Loop Term Bexp Stm -- ^ Iteration of loop body.
    | Scope String (Maybe Integer) Term -- ^ Local variable scope, previous value to restore.
    | Transaction State Bexp Term -- ^ Revertible execution, entry state to revert to.
    | Call [String] [String] State Term -- ^ Procedure body with callee returns, caller returns and local state.
    | Guard State Term String Stm -- ^ Exception handler with entry state.
    | Limit Integer Term -- ^ Bounded execution with remaining steps.
    | Fork Term Term -- ^ Parallel execution interleaving at every step.
    | Weave Term Term -- ^ Alternating execution, left active.
    deriving (Eq, Show)
{- FOURMOLU_ENABLE -}

-- | Machine configuration of term and state.
type Gamma = (Term, State)

-- | Observable effect emitted by a step.
newtype Effect = Output String
    deriving (Eq, Show)

-- | Applied inference rule of a performed step.
{- FOURMOLU_DISABLE -}
data Rule
    = RuleAssign -- ^ Variable definition.
    | RuleSeq -- ^ Sequential composition, left completed.
    | RuleIfTrue -- ^ Conditional, then branch.
    | RuleIfFalse -- ^ Conditional, else branch.
    | RuleWhile -- ^ Loop entry into body.
    | RuleEnd -- ^ Loop exit, guard false.
    | RuleBreak -- ^ Loop exit, break.
    | RulePrint -- ^ Output of evaluation.
    | RuleRead -- ^ Input into variable.
    | RuleLocal -- ^ Local variable scope entry.
    | RuleRestore -- ^ Local variable scope exit.
    | RulePar -- ^ Parallel execution, one side completed.
    | RuleNonDet -- ^ Nondeterministic choice.
    | RuleProcDef -- ^ Procedure definition.
    | RuleInvoke -- ^ Procedure invocation.
    | RuleReturn -- ^ Procedure return to caller.
    | RuleRevert -- ^ Transaction completion.
    | RuleMatch -- ^ Pattern match.
    | RuleHavoc -- ^ Arbitrary variable definition.
    | RulePass -- ^ Assertion check.
    | RuleFlip -- ^ Alternating branching, flip side.
    | RuleFlop -- ^ Alternating branching, flop side.
    | RuleCatch -- ^ Exception caught.
    | RuleSwap -- ^ Variable swap.
    | RuleTimeout -- ^ Timeout expired.
    | RuleAlternate -- ^ Alternating execution, one side completed.
    deriving (Eq, Ord, Show, Enum, Bounded)
{- FOURMOLU_ENABLE -}

-- | One performed transition.
data Step = Step
    { residual :: Gamma
    -- ^ Configuration after the step.
    , effects :: [Effect]
    -- ^ Effects emitted by the step.
    , rule :: Rule
    -- ^ Applied inference rule.
    }
    deriving (Eq, Show)

-- | Result of attempting a transition.
data Configuration
    = -- | Transition performed.
      Stepped Step
    | -- | Blocked on @read@ for variable, continue with value.
      AwaitInput String (Integer -> Configuration)
    | -- | Blocked on @havoc@ for variable, continue with value.
      AwaitHavoc String (Integer -> Configuration)
    | -- | Blocked on nondeterministic choice, continue with 'True' for left.
      AwaitChoice (Bool -> Configuration)
    | -- | No transition, execution completed with outcome.
      Final Flow
    | -- | Execution failed.
      Stuck Exception

-- | Perform a single transition of the configuration.
step :: Gamma -> Configuration
step (term, state) = case term of
    Source stm -> source stm state
    Then t s ->
        descend (\(t', state') -> (Then t' s, state')) (sequential s) Stuck (step (t, state))
    Loop t b s ->
        let exit = \case
                Next state' -> stepped RuleSeq (Source (While b s)) state'
                Broken state' -> stepped RuleBreak (Source Skip) state'
        in descend (\(t', state') -> (Loop t' b s, state')) exit Stuck (step (t, state))
    Scope x old t ->
        let exit = \case
                Next state' -> stepped RuleRestore (Source Skip) (restoreVar state' x old)
                Broken state' -> Final . Broken $ restoreVar state' x old
        in descend (\(t', state') -> (Scope x old t', state')) exit Stuck (step (t, state))
    Transaction entry b t ->
        let exit flow =
                let state' = flowState flow
                in evaluated b state' $ \v ->
                    let final = if v then entry else state'
                    in case flow of
                        Next _ -> stepped RuleRevert (Source Skip) final
                        Broken _ -> Final (Broken final)
        in descend (\(t', state') -> (Transaction entry b t', state')) exit Stuck (step (t, state))
    Call rets returns local t ->
        -- INFO: Callee state lives in the term, interleaved caller steps are unaffected
        let exit = \case
                Next local' -> stepped RuleReturn (Source Skip) (setVars state (zip returns (map (getVar local') rets)))
                Broken _ -> Stuck (Error "break outside loop")
        in descend (\(t', local') -> (Call rets returns local' t', state)) exit Stuck (step (t, local))
    Guard entry t x s ->
        let handler = \case
                Raised v -> stepped RuleCatch (Source s) (setVar entry x v)
                e -> Stuck e
        in descend (\(t', state') -> (Guard entry t' x s, state')) Final handler (step (t, state))
    Limit n t
        | n <= 0 -> stepped RuleTimeout (Source Skip) state
        | otherwise -> descend (\(t', state') -> (Limit (n - 1) t', state')) Final Stuck (step (t, state))
    Fork t1 t2 -> AwaitChoice $ \left ->
        if left
            then descend (\(t1', state') -> (Fork t1' t2, state')) (joined t2) Stuck (step (t1, state))
            else descend (\(t2', state') -> (Fork t1 t2', state')) (joined t1) Stuck (step (t2, state))
    Weave t1 t2 ->
        let exit = \case
                Next state' -> stepped RuleAlternate t2 state'
                broken -> Final broken
        in descend (\(t1', state') -> (Weave t2 t1', state')) exit Stuck (step (t1, state))
    where
        sequential s = \case
            Next state' -> stepped RuleSeq (Source s) state'
            broken -> Final broken
        joined other = \case
            Next state' -> stepped RulePar other state'
            broken -> Final broken

-- | Perform a single transition of a source statement.
source :: Stm -> State -> Configuration
source stm state = case stm of
    Skip -> Final (Next state)
    VarDef x dop a -> evaluated a state $ \v ->
        let
            u = getVar state x
            w = case dop of
                Def -> v
                Inc -> u + v
                Dec -> u - v
                Prod -> u * v
                Quot -> u // v
                Rem -> u %% v
        in stepped RuleAssign (Source Skip) (setVar state x w)
    Seq s1 s2 -> step (Then (Source s1) s2, state)
    IfElse b s1 s2 -> evaluated b state $ \v ->
        if v
            then stepped RuleIfTrue (Source s1) state
            else stepped RuleIfFalse (Source s2) state
    While b s -> evaluated b state $ \v ->
        if v
            then stepped RuleWhile (Loop (Source s) b s) state
            else stepped RuleEnd (Source Skip) state
    Print a -> evaluated a state $ \v ->
        Stepped Step {residual = (Source Skip, state), effects = [Output (show v)], rule = RulePrint}
    Read x -> AwaitInput x $ \v -> stepped RuleRead (Source Skip) (setVar state x v)
    Local x a s -> evaluated a state $ \v ->
        stepped RuleLocal (Scope x (lookupVar state x) (Source s)) (setVar state x v)
    Par s1 s2 -> step (Fork (Source s1) (Source s2), state)
    NonDet s1 s2 -> AwaitChoice $ \left ->
        stepped RuleNonDet (Source (if left then s1 else s2)) state
    ProcDef name params rets body ->
        stepped RuleProcDef (Source Skip) (setProc state name (params, rets, body))
    ProcInvoc name arguments returns -> case getProc state name of
        Nothing -> Stuck . Error $ "undefined procedure: " ++ name
        Just (params, rets, body)
            | length arguments /= length params -> Stuck $ Error "mismatched number of arguments to parameters"
            | length returns /= length rets -> Stuck $ Error "mismatched number of return variables"
            | otherwise -> either Stuck invoke (mapM (`evaluate` state) arguments)
            where
                invoke vals =
                    let local = State (Map.fromList (zip params vals)) (procs state)
                    in stepped RuleInvoke (Call rets returns local (Source body)) state
    Break -> Final (Broken state)
    Revert s b -> step (Transaction state b (Source s), state)
    Match a ms d -> evaluated a state $ \v ->
        stepped RuleMatch (Source (maybe d id (lookup v ms))) state
    Havoc x -> AwaitHavoc x $ \v -> stepped RuleHavoc (Source Skip) (setVar state x v)
    Assert b -> evaluated b state $ \v ->
        if v
            then stepped RulePass (Source Skip) state
            else Stuck . AssertFail $ stringify b
    FlipFlop i s1 s2 ->
        if getFlip state i
            then stepped RuleFlip (Source s1) (setFlop state i)
            else stepped RuleFlop (Source s2) (setFlip state i)
    Raise a -> evaluated a state (Stuck . Raised)
    TryCatch s1 x s2 -> step (Guard state (Source s1) x s2, state)
    Swap x y -> stepped RuleSwap (Source Skip) (setVars state [(x, getVar state y), (y, getVar state x)])
    Timeout s a -> evaluated a state $ \n -> step (Limit n (Source s), state)
    Alternate s1 s2 -> step (Weave (Source s1) (Source s2), state)

-- | Transition without effects.
stepped :: Rule -> Term -> State -> Configuration
stepped r t state = Stepped Step {residual = (t, state), effects = [], rule = r}

-- | Continue with evaluation result or get stuck.
evaluated :: (Evaluate a b) => a -> State -> (b -> Configuration) -> Configuration
evaluated e state k = either Stuck k (evaluate e state)

-- | Descend into the configuration of a subterm: wrap residuals into the
-- enclosing context, thread through blocked continuations, and handle final
-- outcomes and exceptions of the subterm.
descend :: (Gamma -> Gamma) -> (Flow -> Configuration) -> (Exception -> Configuration) -> Configuration -> Configuration
descend wrap final failed = go
    where
        go conf = case conf of
            Stepped s -> Stepped s {residual = wrap (residual s)}
            AwaitInput x k -> AwaitInput x (go . k)
            AwaitHavoc x k -> AwaitHavoc x (go . k)
            AwaitChoice k -> AwaitChoice (go . k)
            Final flow -> final flow
            Stuck e -> failed e

-- | Run configuration to completion, discharging effects in the interpretation monad.
drive :: (MonadIMP m) => Gamma -> m Flow
drive = go . step
    where
        go conf = case conf of
            Stepped s -> do
                mapM_ (\(Output o) -> output o) (effects s)
                tick
                go (step (residual s))
            AwaitInput x k -> input x >>= go . k
            AwaitHavoc _ k -> arbitrary >>= go . k
            AwaitChoice k -> choose >>= go . k
            Final flow -> return flow
            Stuck e -> throwError e
{-# INLINEABLE drive #-}

-- | Execute statement in state to completion.
run :: (MonadIMP m) => Stm -> State -> m Flow
run stm state = drive (Source stm, state)
{-# INLINEABLE run #-}

-- | Maximum number of steps measured by 'measure'.
limit :: Integer
limit = 1000000

-- | Count the steps of executing statement in state, for @time s@.
-- Pure: reads and havocs yield zero, choices take the left branch and output
-- is discarded. Fails if the statement fails or exceeds 'limit' steps.
measure :: Stm -> State -> Either Exception Integer
measure stm state = go 0 (step (Source stm, state))
    where
        go !n conf
            | n > limit = Left . Error $ "time exceeded limit of " ++ show limit ++ " steps"
            | otherwise = case conf of
                Stepped s -> go (n + 1) (step (residual s))
                AwaitInput _ k -> go n (k 0)
                AwaitHavoc _ k -> go n (k 0)
                AwaitChoice k -> go n (k True)
                Final (Next _) -> Right n
                Final (Broken _) -> Left (Error "break outside loop")
                Stuck e -> Left e

-- | Pretty-print term as source with runtime contexts as comments.
instance Pretty Term where
    pretty term = case term of
        Source s -> pretty s
        Then t s -> vsep [pretty t <> semi, pretty s]
        Loop t b s -> vsep [pretty t <> semi, pretty (While b s)]
        Scope x _ t -> vsep [pretty t, pretty ("/* restore " ++ x ++ " */")]
        Transaction _ b t -> vsep [pretty "revert", indent 4 (pretty t), pretty "if" <+> pretty b <+> pretty "end"]
        Call _ returns _ t -> vsep [pretty t, pretty ("/* return to " ++ unwords returns ++ " */")]
        Guard _ t x s -> vsep [pretty "try", indent 4 (pretty t), pretty "catch" <+> pretty x <+> pretty "with", indent 4 (pretty s), pretty "end"]
        Limit n t -> vsep [pretty "timeout", indent 4 (pretty t), pretty "after" <+> pretty n, pretty "end"]
        Fork t1 t2 -> parens (pretty t1) <+> pretty "par" <+> parens (pretty t2)
        Weave t1 t2 -> parens (pretty t1) <+> pretty "alternate" <+> parens (pretty t2)
