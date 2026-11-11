{- |
Module      : IMP.Engine
Description : Runtime session engine over the pure kernel API
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Session management layer over the pure kernel semantics.
Holds the current session (environment, in-flight interpretation, trace and
resource profile), delegates to the kernel, discharges effects through
handlers supplied by frontends, and records history.

The engine itself performs no IO: frontends drive it through 'runEngine' with
their own base monad and 'Handlers'.
-}
module IMP.Engine (
    Aspect (..),
    Semantics (..),
    Profile (..),
    emptyProfile,
    Handlers (..),
    Fault (..),
    Flight (..),
    Session (..),
    Engine,
    runEngine,
    boot,
    snapshot,
    drive,
    begin,
    proceed,
    resetAspect,
    renderAspect,
    renderStep,
    renderProfile,
    traceSource,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)

import qualified Data.Map as Map

import IMP.Exception
import IMP.Kernel
import IMP.Pretty
import IMP.State
import IMP.Syntax

import qualified IMP.Semantics.Natural as Natural

-- | Aspect of the session environment to reset or render.
data Aspect
    = All
    | Vars
    | Procs
    | Flag
    | Trace
    deriving (Eq, Show)

-- | Semantics to interpret statements with.
data Semantics
    = Natural
    | Structural
    deriving (Eq, Show)

-- | Resource profile collected during structural interpretation.
data Profile = Profile
    { stepCount :: Int
    -- ^ Number of steps taken.
    , ruleCount :: Map.Map Rule Int
    -- ^ Applied inference rules.
    , outputCount :: Int
    -- ^ Emitted outputs.
    }
    deriving (Eq, Show)

-- | Empty resource profile.
emptyProfile :: Profile
emptyProfile = Profile {stepCount = 0, ruleCount = Map.empty, outputCount = 0}

-- | Engine-level failure.
data Fault
    = -- | Kernel semantics reported stuck.
      RuntimeFault Exception
    | -- | Resolution does not match the current suspension.
      InvalidResolution Gamma Resolution
    | -- | No interpretation currently running or suspended.
      NoFlight
    | -- | Interpretation already running or suspended.
      BusyFlight
    | -- | Interpretation interrupted on frontend request.
      Interrupted
    deriving (Eq)

-- | Instance of 'Show' with severity marker followed by content.
instance Show Fault where
    show fault = case fault of
        RuntimeFault e -> show e
        InvalidResolution _ resolution -> "*** ERROR: invalid resolution: " ++ show resolution
        NoFlight -> "*** ERROR: no interpretation in progress"
        BusyFlight -> "*** ERROR: interpretation already in progress"
        Interrupted -> "*** ERROR: interrupted"

-- | In-flight interpretation under the structural semantics.
data Flight = Flight
    { statement :: Stm
    -- ^ Statement under interpretation.
    , focus :: Gamma
    -- ^ Current configuration focus.
    , blocked :: Configuration
    -- ^ Latest configuration of the focus.
    }
    deriving (Eq, Show)

-- | Session state managed by the engine.
data Session = Session
    { environment :: State
    -- ^ Committed interpreter environment.
    , flight :: Maybe Flight
    -- ^ In-flight interpretation, if any.
    , trace :: [Stm]
    -- ^ Executed statements, newest first.
    , stepHistory :: [Step]
    -- ^ Steps taken by the latest interpretation, newest first.
    , profile :: Profile
    -- ^ Resource profile of the latest interpretation.
    }
    deriving (Eq, Show)

-- | Engine monad stack over abstract base monad.
type Engine m = ReaderT (Handlers m) (StateT Session (ExceptT Fault m))

-- | Lift an action of the base monad into the engine monad.
hoistEngine :: (Monad m) => m a -> Engine m a
hoistEngine = lift . lift . lift

-- | Run engine computation with handlers and session over base monad.
runEngine :: Handlers m -> Session -> Engine m a -> m (Either Fault (a, Session))
runEngine handlers session action = runExceptT $ runStateT (runReaderT action handlers) session

-- | Initial empty session.
boot :: Session
boot =
    Session
        { environment = initial
        , flight = Nothing
        , trace = []
        , stepHistory = []
        , profile = emptyProfile
        }

-- | Read current session.
snapshot :: (Monad m) => Engine m Session
snapshot = get

-- | Interpret statement to completion, discharging suspensions and effects with handlers.
drive :: (Monad m) => Semantics -> Stm -> Engine m Configuration
drive semantics stm = case semantics of
    Natural -> do
        handlers <- ask
        st <- gets environment
        modify $ \session -> session {stepHistory = [], profile = emptyProfile}
        result <- hoistEngine $ runExceptT $ Natural.run handlers (stm, st)
        case result of
            Left e -> throwError (RuntimeFault e)
            Right state' -> do
                modify $ \session -> session {environment = state', trace = stm : trace session}
                return (Terminal state')
    Structural -> begin stm >>= drain

-- | Begin structural interpretation of statement, stepping until the first pause point.
begin :: (Monad m) => Stm -> Engine m Configuration
begin stm = do
    session <- get
    case flight session of
        Just _ -> throwError BusyFlight
        Nothing -> return ()
    let gamma = (stm, environment session)
    let conf = step gamma
    put
        session
            { flight = Just Flight {statement = stm, focus = gamma, blocked = conf}
            , stepHistory = []
            , profile = emptyProfile
            }
    pause conf

-- | Resolve the current suspension and continue to the next pause point.
proceed :: (Monad m) => Resolution -> Engine m Configuration
proceed resolution = do
    session <- get
    case flight session of
        Nothing -> throwError NoFlight
        Just fl ->
            case blocked fl of
                Blocked _ -> do
                    let conf = resume (focus fl) resolution
                    modify $ \flightsession -> flightsession {flight = Just fl {blocked = conf}}
                    pause conf
                _ -> throwError (InvalidResolution (focus fl) resolution)

-- | Reset aspect of the session environment.
resetAspect :: (Monad m) => Aspect -> Engine m ()
resetAspect aspect = modify $ \session -> case aspect of
    All -> session {environment = initial, trace = []}
    Vars -> session {environment = resetVars (environment session)}
    Procs -> session {environment = resetProcs (environment session)}
    Flag -> session {environment = resetBreak (environment session)}
    Trace -> session {trace = []}

-- | Render aspect of the session environment.
renderAspect :: Session -> Aspect -> String
renderAspect session aspect = case aspect of
    All -> unlines $ map (renderAspect session) [Vars, Procs, Flag, Trace]
    Vars ->
        explain
            "Variables:"
            -- INFO: Invariant of IMP.State.setVar guarantees no empty string key
            [k ++ " = " ++ show v | (k, v) <- Map.toList vars, visible k]
    Procs ->
        explain
            "Procedures:"
            [ prettify (ProcDef name params rets body)
            | (name, (params, rets, body)) <- Map.toList procs
            ]
    Flag -> "Break: " ++ if flag then "true" else "false"
    Trace ->
        explain
            "Trace:"
            [ init . unlines $ zipWith (++) (index : buffer) (lines s)
            | (i, s) <- zip [1 :: Int ..] ((reverse . map prettify) (trace session))
            , let
                index = '#' : show i ++ space 2
                buffer = repeat . space $ length (show i) + 3
            ]
    where
        (vars, procs, flag) = environment session
        visible ('_' : _) = False
        visible _ = True

-- | Render performed step for debug output.
renderStep :: Step -> String
renderStep s' = unwords [show (rule s'), "=>", stringify (fst (residual s'))]

-- | Render resource profile of the latest interpretation.
renderProfile :: Profile -> String
renderProfile prof =
    unwords $
        [show (stepCount prof), "steps"]
            ++ [show r ++ " x" ++ show n | (r, n) <- Map.toList (ruleCount prof)]
            ++ [show (outputCount prof), "outputs"]

-- | Source text of trace, oldest statement first.
traceSource :: Session -> String
traceSource = prettytrace . reverse . trace

-- | Discharge suspension with handlers until interpretation completes.
drain :: (Monad m) => Configuration -> Engine m Configuration
drain conf = case conf of
    Blocked suspension -> do
        handlers <- ask
        resolution <- hoistEngine $ case suspension of
            AwaitInput x -> ResolveInput <$> onInput handlers x
            AwaitHavoc _ -> ResolveHavoc <$> onRandom handlers
            AwaitChoice -> ResolveChoice <$> onChoice handlers
        conf' <- proceed resolution
        drain conf'
    _ -> return conf

-- | Advance structural interpretation until the next pause point.
spin :: (Monad m) => Engine m Configuration
spin = loop
    where
        loop = do
            session <- get
            case flight session of
                Nothing -> throwError NoFlight
                Just fl ->
                    case blocked fl of
                        Stepped s' -> do
                            handlers <- ask
                            interrupted <- hoistEngine (pollInterrupt handlers)
                            if interrupted
                                then do
                                    modify $ \s -> s {flight = Nothing}
                                    throwError Interrupted
                                else do
                                    record s'
                                    mapM_ (\(Output text) -> hoistEngine (onOutput handlers text)) (effects s')
                                    let gamma = residual s'
                                    let conf = step gamma
                                    modify $ \s -> s {flight = Just fl {focus = gamma, blocked = conf}}
                                    pause conf
                        conf -> pause conf

-- | Handle reached pause point: advance on steps, commit on terminal, fail on stuck.
pause :: (Monad m) => Configuration -> Engine m Configuration
pause conf = case conf of
    Stepped _ -> spin
    Terminal state' -> do
        session <- get
        case flight session of
            Just fl ->
                put
                    session
                        { environment = state'
                        , flight = Nothing
                        , trace = statement fl : trace session
                        }
            Nothing -> return ()
        return conf
    Stuck e -> do
        modify $ \session -> session {flight = Nothing}
        throwError (RuntimeFault e)
    Blocked _ -> return conf

-- | Record performed step in history and resource profile.
record :: (Monad m) => Step -> Engine m ()
record s' = modify $ \session ->
    session
        { stepHistory = s' : stepHistory session
        , profile =
            (profile session)
                { stepCount = stepCount (profile session) + 1
                , ruleCount = Map.insertWith (+) (rule s') 1 (ruleCount (profile session))
                , outputCount = outputCount (profile session) + length (effects s')
                }
        }

-- | Render heading with indented body lines.
explain :: String -> [String] -> String
explain heading [] = heading
explain heading body = heading ++ '\n' : indent 4 (unlines body)

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
