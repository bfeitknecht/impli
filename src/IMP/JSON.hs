{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IMP.JSON
Description : JSON serialization for the IMP language kernel and protocol
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

JSON serialization (aeson) of kernel configurations, syntax trees, states and
exceptions, plus the protocol envelopes exchanged with the web frontend.
Encoding only: the frontend sends plain strings and the backend answers JSON.
-}
module IMP.JSON (
    TraceEntry (..),
    Envelope (..),
    Status (..),
    Steps (..),
) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import qualified Data.Map as Map

import IMP.Engine
import IMP.Exception
import IMP.Kernel
import IMP.State
import IMP.Syntax

-- | Entry of the statement trace for the web frontend.
data TraceEntry = TraceEntry
    { traceIndex :: Int
    -- ^ Index from newest statement backwards, starting at one.
    , traceSource' :: String
    -- ^ Pretty-printed source text.
    , traceAst :: Stm
    -- ^ Abstract syntax tree.
    }

-- | Events and status envelope exchanged with the web frontend.
data Envelope = Envelope
    { envelopeEvents :: [String]
    -- ^ Output lines emitted during interpretation.
    , envelopeStatus :: Status
    -- ^ Status of the interpretation.
    }

-- | Status of an interpretation for the web protocol.
data Status
    = -- | Interpretation completed with resulting state.
      Done State Profile
    | -- | Blocked on suspension with optional focus statement and pretty-printed
      -- source, flag whether retry after invalid resolution.
      Awaiting Suspension (Maybe (Stm, String)) Bool
    | -- | Interpretation faulted.
      Failed Fault

-- | Steps taken by the latest interpretation for the inference tree tab.
data Steps = Steps
    { stepsTaken :: [Step]
    -- ^ Steps in chronological order.
    , stepsProfile :: Profile
    -- ^ Resource profile.
    }

-- | Apply tag with fields to JSON object.
tag :: Text -> [Pair] -> Value
tag name fields = object $ ("tag" .= name) : fields

instance ToJSON Aop where
    toJSON aop = case aop of
        Add -> "add"
        Sub -> "sub"
        Mul -> "mul"
        Div -> "div"
        Mod -> "mod"

instance ToJSON Rop where
    toJSON rop = case rop of
        Eq -> "eq"
        Neq -> "neq"
        Lt -> "lt"
        Leq -> "leq"
        Gt -> "gt"
        Geq -> "geq"

instance ToJSON Dop where
    toJSON dop = case dop of
        Def -> "def"
        Inc -> "inc"
        Dec -> "dec"
        Prod -> "prod"
        Quot -> "quot"
        Rem -> "rem"

instance ToJSON Aexp where
    toJSON aexp = case aexp of
        Val v -> tag "val" ["value" .= v]
        Var x -> tag "var" ["name" .= x]
        Bin aop left right -> tag "bin" ["op" .= aop, "left" .= left, "right" .= right]
        Time s -> tag "time" ["statement" .= s]

instance ToJSON Bexp where
    toJSON bexp = case bexp of
        Lit b -> tag "lit" ["value" .= b]
        Or left right -> tag "or" ["left" .= left, "right" .= right]
        And left right -> tag "and" ["left" .= left, "right" .= right]
        Not b -> tag "not" ["expr" .= b]
        Rel rop left right -> tag "rel" ["op" .= rop, "left" .= left, "right" .= right]

instance {-# OVERLAPPING #-} ToJSON Proc where
    toJSON (params, rets, body) =
        object ["params" .= params, "returns" .= rets, "body" .= body]

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
        Restore _ -> tag "restore" []
        Return _ rets returns -> tag "return" ["rets" .= rets, "returns" .= returns]
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

instance ToJSON Construct where
    toJSON construct = case construct of
        Statement s -> tag "statement" ["statement" .= s]
        Arithmetic a -> tag "arithmetic" ["expr" .= a]
        Boolean b -> tag "boolean" ["expr" .= b]
        Whitespace -> tag "whitespace" []

instance {-# OVERLAPPING #-} ToJSON State where
    toJSON (vars, procs, flag) =
        object
            [ "vars" .= Map.filterWithKey (\k _ -> notHidden k) vars
            , "procs" .= procs
            , "break" .= flag
            ]
        where
            -- INFO: Invariant of IMP.State.setVar guarantees no empty string key
            notHidden ('_' : _) = False
            notHidden _ = True

instance ToJSON Exception where
    toJSON e = case e of
        ParseFail msg -> tag "parse-failure" ["message" .= msg]
        AssertFail msg -> tag "assert-failure" ["message" .= msg]
        IOFail msg -> tag "io-failure" ["message" .= msg]
        Raised v -> tag "raised" ["value" .= v]
        Error msg -> tag "error" ["message" .= msg]
        Info msg -> tag "info" ["message" .= msg]
        Empty -> tag "empty" []

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

instance ToJSON Effect where
    toJSON (Output text) = toJSON text

instance ToJSON Suspension where
    toJSON suspension = case suspension of
        AwaitInput x -> tag "await-input" ["variable" .= x]
        AwaitHavoc x -> tag "await-havoc" ["variable" .= x]
        AwaitChoice -> tag "await-choice" []

instance ToJSON Step where
    toJSON s' =
        object
            [ "rule" .= rule s'
            , "statement" .= fst (residual s')
            , "state" .= snd (residual s')
            , "effects" .= effects s'
            ]

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

instance ToJSON TraceEntry where
    toJSON entry =
        object
            [ "index" .= traceIndex entry
            , "source" .= traceSource' entry
            , "ast" .= traceAst entry
            ]

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
    toJSON (Failed fault) = tag "error" ["fault" .= fault]

instance ToJSON Envelope where
    toJSON envelope =
        object ["events" .= envelopeEvents envelope, "status" .= envelopeStatus envelope]

instance ToJSON Steps where
    toJSON steps = object ["steps" .= stepsTaken steps, "profile" .= stepsProfile steps]
