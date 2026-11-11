{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IMP.JSON
Description : JSON serialization for the IMP language kernel and protocol
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

JSON serialization (aeson) of kernel configurations, syntax trees, states,
sessions and exceptions, plus the protocol envelopes exchanged with the web
frontend. Sessions round-trip: the JavaScript side owns the session between
API calls and hands it back to the next call as plain JSON. Step histories
and resource profiles are deliberately not part of the session encoding; they
are per-interpretation deltas streamed in the 'steps' field of envelopes.
-}
module IMP.JSON (
    Envelope (..),
    Status (..),
) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as Text

import IMP.Engine
import IMP.Exception
import IMP.Kernel
import IMP.State
import IMP.Syntax

-- | Events, streamed steps, status and updated session envelope exchanged with the frontend.
data Envelope = Envelope
    { envelopeEvents :: [String]
    -- ^ Output lines emitted during interpretation.
    , envelopeStatus :: Status
    -- ^ Status of the interpretation.
    , envelopeSteps :: [Step]
    -- ^ Steps taken during this call, in chronological order.
    , envelopeSession :: Session
    -- ^ Session state after this call, owned by the JavaScript side.
    }

-- | Status of an interpretation for the web protocol.
data Status
    = -- | Interpretation completed with resulting state.
      Done State Profile
    | -- | Blocked on suspension with optional focus statement and pretty-printed
      -- source, flag whether retry after invalid resolution.
      Awaiting Suspension (Maybe (Stm, String)) Bool
    | -- | Mid-flight interpretation paused on fuel exhaustion; continue with @advance@.
      Running
    | -- | Interpretation faulted.
      Failed Fault

-- | Apply tag with fields to JSON object.
tag :: Text -> [Pair] -> Value
tag name fields = object $ ("tag" .= name) : fields

-- | Read the tag of a tagged JSON object.
tagOf :: Object -> Parser Text
tagOf object = object .: "tag"

instance ToJSON Aop where
    toJSON aop = case aop of
        Add -> "add"
        Sub -> "sub"
        Mul -> "mul"
        Div -> "div"
        Mod -> "mod"

instance FromJSON Aop where
    parseJSON = withText "Aop" $ \text -> case text of
        "add" -> pure Add
        "sub" -> pure Sub
        "mul" -> pure Mul
        "div" -> pure Div
        "mod" -> pure Mod
        other -> fail $ "unknown arithmetic operator: " ++ show other

instance ToJSON Rop where
    toJSON rop = case rop of
        Eq -> "eq"
        Neq -> "neq"
        Lt -> "lt"
        Leq -> "leq"
        Gt -> "gt"
        Geq -> "geq"

instance FromJSON Rop where
    parseJSON = withText "Rop" $ \text -> case text of
        "eq" -> pure Eq
        "neq" -> pure Neq
        "lt" -> pure Lt
        "leq" -> pure Leq
        "gt" -> pure Gt
        "geq" -> pure Geq
        other -> fail $ "unknown relational operator: " ++ show other

instance ToJSON Dop where
    toJSON dop = case dop of
        Def -> "def"
        Inc -> "inc"
        Dec -> "dec"
        Prod -> "prod"
        Quot -> "quot"
        Rem -> "rem"

instance FromJSON Dop where
    parseJSON = withText "Dop" $ \text -> case text of
        "def" -> pure Def
        "inc" -> pure Inc
        "dec" -> pure Dec
        "prod" -> pure Prod
        "quot" -> pure Quot
        "rem" -> pure Rem
        other -> fail $ "unknown definition operator: " ++ show other

instance ToJSON Aexp where
    toJSON aexp = case aexp of
        Val v -> tag "val" ["value" .= v]
        Var x -> tag "var" ["name" .= x]
        Bin aop left right -> tag "bin" ["op" .= aop, "left" .= left, "right" .= right]
        Time s -> tag "time" ["statement" .= s]

instance FromJSON Aexp where
    parseJSON = withObject "Aexp" $ \object ->
        tagOf object >>= \case
            "val" -> Val <$> object .: "value"
            "var" -> Var <$> object .: "name"
            "bin" -> Bin <$> object .: "op" <*> object .: "left" <*> object .: "right"
            "time" -> Time <$> object .: "statement"
            other -> fail $ "unknown arithmetic expression: " ++ show other

instance ToJSON Bexp where
    toJSON bexp = case bexp of
        Lit b -> tag "lit" ["value" .= b]
        Or left right -> tag "or" ["left" .= left, "right" .= right]
        And left right -> tag "and" ["left" .= left, "right" .= right]
        Not b -> tag "not" ["expr" .= b]
        Rel rop left right -> tag "rel" ["op" .= rop, "left" .= left, "right" .= right]

instance FromJSON Bexp where
    parseJSON = withObject "Bexp" $ \object ->
        tagOf object >>= \case
            "lit" -> Lit <$> object .: "value"
            "or" -> Or <$> object .: "left" <*> object .: "right"
            "and" -> And <$> object .: "left" <*> object .: "right"
            "not" -> Not <$> object .: "expr"
            "rel" -> Rel <$> object .: "op" <*> object .: "left" <*> object .: "right"
            other -> fail $ "unknown boolean expression: " ++ show other

instance {-# OVERLAPPING #-} ToJSON Proc where
    toJSON (params, rets, body) =
        object ["params" .= params, "returns" .= rets, "body" .= body]

instance {-# OVERLAPPING #-} FromJSON Proc where
    parseJSON = withObject "Proc" $ \object ->
        (,,) <$> object .: "params" <*> object .: "returns" <*> object .: "body"

-- | Encode state snapshot for 'Restore' and 'Return' statements.
snapshotFields :: Snapshot -> [Pair]
snapshotFields (vars, procs, flag) =
    [ "vars" .= Map.fromList vars
    , "procs" .= Map.fromList procs
    , "break" .= flag
    ]

-- | Decode state snapshot for 'Restore' and 'Return' statements.
parseSnapshot :: Object -> Parser Snapshot
parseSnapshot object = do
    vars <- object .: "vars"
    procs <- object .: "procs"
    flag <- object .: "break"
    pure (Map.toList vars, Map.toList procs, flag)

instance ToJSON Stm where
    toJSON stm = case stm of
        Skip -> tag "skip" []
        VarDef x dop a -> tag "vardef" ["var" .= x, "op" .= dop, "expr" .= a]
        Seq s1 s2 -> tag "seq" ["first" .= s1, "second" .= s2]
        IfElse b s1 s2 -> tag "ifelse" ["condition" .= b, "then" .= s1, "else" .= s2]
        While b s -> tag "while" ["condition" .= b, "body" .= s]
        Print a -> tag "print" ["expr" .= a]
        Read x -> tag "read" ["var" .= x]
        Local x a s -> tag "local" ["var" .= x, "expr" .= a, "body" .= s]
        Par s1 s2 -> tag "par" ["left" .= s1, "right" .= s2]
        NonDet s1 s2 -> tag "nondet" ["left" .= s1, "right" .= s2]
        ProcDef name params rets body ->
            tag "procdef" ["name" .= name, "params" .= params, "returns" .= rets, "body" .= body]
        ProcInvoc name arguments returns ->
            tag "procinvoc" ["name" .= name, "arguments" .= arguments, "returns" .= returns]
        Restore snapshot -> tag "restore" ["snapshot" .= object (snapshotFields snapshot)]
        Return section rets returns ->
            tag "return" ["snapshot" .= object (snapshotFields section), "rets" .= rets, "returns" .= returns]
        Break -> tag "break" []
        Revert s b -> tag "revert" ["body" .= s, "condition" .= b]
        Match a ms d ->
            tag "match" ["subject" .= a, "branches" .= map pair ms, "default" .= d]
            where
                pair (v, s) = object ["value" .= v, "statement" .= s]
        Havoc x -> tag "havoc" ["var" .= x]
        Assert b -> tag "assert" ["condition" .= b]
        FlipFlop i s1 s2 -> tag "flipflop" ["index" .= i, "flip" .= s1, "flop" .= s2]
        Raise a -> tag "raise" ["expr" .= a]
        TryCatch s1 x s2 -> tag "trycatch" ["body" .= s1, "var" .= x, "handler" .= s2]
        Swap x y -> tag "swap" ["left" .= x, "right" .= y]
        Timeout s a -> tag "timeout" ["body" .= s, "budget" .= a]
        Alternate s1 s2 -> tag "alternate" ["first" .= s1, "second" .= s2]

instance FromJSON Stm where
    parseJSON = withObject "Stm" $ \object ->
        tagOf object >>= \case
            "skip" -> pure Skip
            "vardef" -> VarDef <$> object .: "var" <*> object .: "op" <*> object .: "expr"
            "seq" -> Seq <$> object .: "first" <*> object .: "second"
            "ifelse" -> IfElse <$> object .: "condition" <*> object .: "then" <*> object .: "else"
            "while" -> While <$> object .: "condition" <*> object .: "body"
            "print" -> Print <$> object .: "expr"
            "read" -> Read <$> object .: "var"
            "local" -> Local <$> object .: "var" <*> object .: "expr" <*> object .: "body"
            "par" -> Par <$> object .: "left" <*> object .: "right"
            "nondet" -> NonDet <$> object .: "left" <*> object .: "right"
            "procdef" ->
                ProcDef <$> object .: "name" <*> object .: "params" <*> object .: "returns" <*> object .: "body"
            "procinvoc" ->
                ProcInvoc <$> object .: "name" <*> object .: "arguments" <*> object .: "returns"
            "restore" -> Restore <$> (object .: "snapshot" >>= withObject "Snapshot" parseSnapshot)
            "return" ->
                Return
                    <$> (object .: "snapshot" >>= withObject "Snapshot" parseSnapshot)
                    <*> object .: "rets"
                    <*> object .: "returns"
            "break" -> pure Break
            "revert" -> Revert <$> object .: "body" <*> object .: "condition"
            "match" -> do
                subject <- object .: "subject"
                branches <- object .: "branches"
                pairs <- mapM pair branches
                Match subject pairs <$> object .: "default"
                where
                    pair = withObject "branch" $ \branch ->
                        (,) <$> branch .: "value" <*> branch .: "statement"
            "havoc" -> Havoc <$> object .: "var"
            "assert" -> Assert <$> object .: "condition"
            "flipflop" -> FlipFlop <$> object .: "index" <*> object .: "flip" <*> object .: "flop"
            "raise" -> Raise <$> object .: "expr"
            "trycatch" -> TryCatch <$> object .: "body" <*> object .: "var" <*> object .: "handler"
            "swap" -> Swap <$> object .: "left" <*> object .: "right"
            "timeout" -> Timeout <$> object .: "body" <*> object .: "budget"
            "alternate" -> Alternate <$> object .: "first" <*> object .: "second"
            other -> fail $ "unknown statement: " ++ show other

instance ToJSON Construct where
    toJSON construct = case construct of
        Statement s -> tag "statement" ["statement" .= s]
        Arithmetic a -> tag "arithmetic" ["expr" .= a]
        Boolean b -> tag "boolean" ["expr" .= b]
        Whitespace -> tag "whitespace" []

-- INFO: Round-trips must preserve internal ('_'-prefixed) bookkeeping
-- variables, so the raw encoding does not filter; display-side filtering is
-- the frontend's concern.
instance {-# OVERLAPPING #-} ToJSON State where
    toJSON (vars, procs, flag) =
        object ["vars" .= vars, "procs" .= procs, "break" .= flag]

instance {-# OVERLAPPING #-} FromJSON State where
    parseJSON = withObject "State" $ \object ->
        (,,) <$> object .: "vars" <*> object .: "procs" <*> object .: "break"

instance ToJSON Exception where
    toJSON e = case e of
        ParseFail msg -> tag "parse-failure" ["message" .= msg]
        AssertFail msg -> tag "assert-failure" ["message" .= msg]
        IOFail msg -> tag "io-failure" ["message" .= msg]
        Raised v -> tag "raised" ["value" .= v]
        Error msg -> tag "error" ["message" .= msg]
        Info msg -> tag "info" ["message" .= msg]
        Empty -> tag "empty" []

instance FromJSON Exception where
    parseJSON = withObject "Exception" $ \object ->
        tagOf object >>= \case
            "parse-failure" -> ParseFail <$> object .: "message"
            "assert-failure" -> AssertFail <$> object .: "message"
            "io-failure" -> IOFail <$> object .: "message"
            "raised" -> Raised <$> object .: "value"
            "error" -> Error <$> object .: "message"
            "info" -> Info <$> object .: "message"
            "empty" -> pure Empty
            other -> fail $ "unknown exception: " ++ show other

instance ToJSON Rule where
    toJSON r = toJSON $ case r of
        RuleVarDef -> "var-def"
        RuleSeq -> "seq"
        RuleIfThen -> "if-then"
        RuleIfElse -> "if-else"
        RuleWhile -> "while"
        RulePrint -> "print"
        RuleRead -> "read"
        RuleLocal -> "local"
        RuleChoice -> "choice"
        RuleProcDef -> "proc-def"
        RuleProcInvoc -> "proc-invoc"
        RuleRestore -> "restore"
        RuleReturn -> "return"
        RuleBreak -> "break"
        RuleRevert -> "revert"
        RuleMatch -> "match"
        RuleHavoc -> "havoc"
        RuleAssert -> "assert"
        RuleFlip -> "flip"
        RuleFlop -> "flop"
        RuleRaise -> "raise"
        RuleTryCatch -> "try-catch"
        RuleSwap -> "swap"
        RuleTimeout -> "timeout"
        RuleAlternate -> "alternate"

instance FromJSON Rule where
    parseJSON = withText "Rule" $ \text -> case text of
        "var-def" -> pure RuleVarDef
        "seq" -> pure RuleSeq
        "if-then" -> pure RuleIfThen
        "if-else" -> pure RuleIfElse
        "while" -> pure RuleWhile
        "print" -> pure RulePrint
        "read" -> pure RuleRead
        "local" -> pure RuleLocal
        "choice" -> pure RuleChoice
        "proc-def" -> pure RuleProcDef
        "proc-invoc" -> pure RuleProcInvoc
        "restore" -> pure RuleRestore
        "return" -> pure RuleReturn
        "break" -> pure RuleBreak
        "revert" -> pure RuleRevert
        "match" -> pure RuleMatch
        "havoc" -> pure RuleHavoc
        "assert" -> pure RuleAssert
        "flip" -> pure RuleFlip
        "flop" -> pure RuleFlop
        "raise" -> pure RuleRaise
        "try-catch" -> pure RuleTryCatch
        "swap" -> pure RuleSwap
        "timeout" -> pure RuleTimeout
        "alternate" -> pure RuleAlternate
        other -> fail $ "unknown rule: " ++ show other

instance ToJSON Effect where
    toJSON (Output text) = toJSON text

instance FromJSON Effect where
    parseJSON = fmap Output . parseJSON

instance ToJSON Suspension where
    toJSON suspension = case suspension of
        AwaitInput x -> tag "await-input" ["variable" .= x]
        AwaitHavoc x -> tag "await-havoc" ["variable" .= x]
        AwaitChoice -> tag "await-choice" []

instance FromJSON Suspension where
    parseJSON = withObject "Suspension" $ \object ->
        tagOf object >>= \case
            "await-input" -> AwaitInput <$> object .: "variable"
            "await-havoc" -> AwaitHavoc <$> object .: "variable"
            "await-choice" -> pure AwaitChoice
            other -> fail $ "unknown suspension: " ++ show other

instance ToJSON Step where
    toJSON s' =
        object
            [ "rule" .= rule s'
            , "statement" .= fst (residual s')
            , "state" .= snd (residual s')
            , "effects" .= effects s'
            ]

instance FromJSON Step where
    parseJSON = withObject "Step" $ \object -> do
        rule' <- object .: "rule"
        stm <- object .: "statement"
        state <- object .: "state"
        effects' <- object .: "effects"
        pure Step {rule = rule', residual = (stm, state), effects = effects'}

instance ToJSON Configuration where
    toJSON conf = case conf of
        Terminal state -> tag "terminal" ["state" .= state]
        Stepped s' -> tag "stepped" ["step" .= s']
        Blocked suspension -> tag "blocked" ["suspension" .= suspension]
        Stuck e -> tag "stuck" ["exception" .= e]

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \object ->
        tagOf object >>= \case
            "terminal" -> Terminal <$> object .: "state"
            "stepped" -> Stepped <$> object .: "step"
            "blocked" -> Blocked <$> object .: "suspension"
            "stuck" -> Stuck <$> object .: "exception"
            other -> fail $ "unknown configuration: " ++ show other

instance {-# OVERLAPPING #-} ToJSON Gamma where
    toJSON gamma = object ["statement" .= fst gamma, "state" .= snd gamma]

instance {-# OVERLAPPING #-} FromJSON Gamma where
    parseJSON = withObject "Gamma" $ \object ->
        (,) <$> object .: "statement" <*> object .: "state"

instance ToJSON Flight where
    toJSON fl =
        object
            [ "statement" .= statement fl
            , "focus" .= focus fl
            , "blocked" .= blocked fl
            ]

instance FromJSON Flight where
    parseJSON = withObject "Flight" $ \object ->
        Flight <$> object .: "statement" <*> object .: "focus" <*> object .: "blocked"

-- INFO: Step history and resource profile are per-call deltas streamed in the
-- envelope; only environment, in-flight interpretation and trace round-trip.
instance ToJSON Session where
    toJSON session =
        object
            [ "environment" .= environment session
            , "flight" .= flight session
            , "trace" .= trace session
            ]

instance FromJSON Session where
    parseJSON = withObject "Session" $ \object -> do
        env <- object .: "environment"
        fl <- object .: "flight"
        tr <- object .: "trace"
        pure
            Session
                { environment = env
                , flight = fl
                , trace = tr
                , stepHistory = []
                , profile = emptyProfile
                }

instance ToJSON Profile where
    toJSON prof =
        object
            [ "steps" .= stepCount prof
            , "outputs" .= outputCount prof
            , "rules" .= [object ["rule" .= r, "count" .= n] | (r, n) <- Map.toList (ruleCount prof)]
            ]

instance ToJSON Fault where
    toJSON fault = case fault of
        RuntimeFault e -> tag "runtime" ["exception" .= e]
        InvalidResolution _ resolution -> tag "invalid-resolution" ["resolution" .= show resolution]
        NoFlight -> tag "no-flight" []
        BusyFlight -> tag "busy" []
        Interrupted -> tag "interrupted" []

instance ToJSON Status where
    toJSON (Done state prof) = tag "done" ["state" .= state, "profile" .= prof]
    toJSON (Awaiting suspension focus retry) = object $ fields suspension ++ focused focus
        where
            fields suspension = case suspension of
                AwaitInput x -> ["tag" .= ("await-input" :: Text), "variable" .= x]
                AwaitHavoc x -> ["tag" .= ("await-havoc" :: Text), "variable" .= x]
                AwaitChoice -> ["tag" .= ("await-choice" :: Text)]
            focused (Just (stm, source)) = ["retry" .= retry, "focus" .= source, "stm" .= stm]
            focused Nothing = ["retry" .= retry]
    toJSON Running = tag "running" []
    toJSON (Failed fault) = tag "error" ["fault" .= fault]

instance ToJSON Envelope where
    toJSON envelope =
        object
            [ "events" .= envelopeEvents envelope
            , "status" .= envelopeStatus envelope
            , "steps" .= envelopeSteps envelope
            , "session" .= envelopeSession envelope
            ]
