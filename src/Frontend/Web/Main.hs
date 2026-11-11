{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : WASM API entrypoint for the JavaScript-driven IMP frontend
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : WASM

Exposes an export-only WASM interface over 'IMP.Engine': no foreign imports
and no shared interpreter state. The JavaScript frontend owns the session as
plain JSON, hands it to every API call and adopts the updated session carried
in every envelope, which makes sessions persistable and resumable outside the
interpreter. Statements are interpreted under a step budget (fuel): when a
call pauses mid-flight with status @running@, the frontend yields to its
event loop and calls @advance@ to continue, which keeps Ctrl-C interruption
responsive at any point during interpretation, not only at read prompts. To
cancel an idle suspended interpretation the frontend calls @cancel@.

The @start@ export runs a naive stdio REPL usable in wasmtime and friends,
handling only plain statements and a minimal set of metacommands.
-}
module Main where

import Control.Monad.Except (runExceptT)
import Data.Aeson hiding (Error)
import Data.IORef
import Data.Version (showVersion)
import GHC.Wasm.Prim
import System.IO
import System.Random (randomIO)
import Text.Parsec (ParseError)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy.Char8 as ByteString

import IMP.Engine
import IMP.Exception (Exception (..))
import IMP.Expression
import IMP.JSON
import IMP.Kernel
import IMP.Parser
import IMP.Pretty (prettify)
import IMP.Syntax

import qualified IMP.Semantics.Natural.Derive as Derive
import qualified Paths_impli as Paths

-- | Handlers over IO for explicit browser driving with call-scoped output buffering.
-- Suspensions are never discharged here: interpretation pauses at blocked
-- configurations and the frontend continues via the resolution exports.
webHandlers :: IORef [String] -> Handlers IO
webHandlers outputs =
    Handlers
        { onInput = \_ -> error "browser discharges input suspensions via resolveInput"
        , onOutput = \text -> modifyIORef outputs (text :)
        , onChoice = error "browser discharges choice suspensions via resolveChoice"
        , onRandom = error "browser discharges havoc suspensions via resolveHavoc"
        , pollInterrupt = return False
        }

-- | Handlers over standard IO for the naive stdio REPL.
stdioHandlers :: Handlers IO
stdioHandlers =
    Handlers
        { onInput = \x -> putStr (x ++ " := ") >> hFlush stdout >> fmap read getLine
        , onOutput = putStrLn
        , onChoice = randomIO
        , onRandom = randomIO
        , pollInterrupt = return False
        }

-- | Version string.
versionText :: String
versionText = unwords ["impli", showVersion Paths.version]

-- | Help text for the metacommands handled by the JavaScript frontend.
webHelp :: String
webHelp =
    unlines $
        "All metacommands besides :set can be abbreviated by their first letter"
            : map ("    " ++) entries
    where
        entries =
            [ ":help                    Show this help message"
            , ":tips                    Show some tips"
            , ":quit                    Clear state and disable the terminal"
            , ":clear                   Clear screen"
            , ":version                 Show version"
            , ":reset [ASPECT]         Reset environment or specific aspect (vars, procs, break, trace)"
            , ":show [ASPECT]          Show environment or specific aspect"
            , ":load FILE              Interpret bundled example file"
            , ":write FILE             Write trace to plaintext file in new browser tab"
            , ":ast (INPUT | #n)       Parse and display AST of input or n-th statement in trace"
            ]

-- | Default step budget per call: large enough to feel direct, small enough to
-- keep the event loop of the JavaScript host responsive for interruption.
defaultFuel :: Int
defaultFuel = 16384

-- | Parse a fuel argument, defaulting on junk.
parseFuel :: JSString -> Int
parseFuel raw = maybe defaultFuel id (readMaybe (fromJSString raw))

-- | Encode value as JSON across the FFI boundary.
jsonOut :: (ToJSON a) => a -> IO JSString
jsonOut = return . toJSString . ByteString.unpack . encode

-- | Decode the session handed down by the JavaScript frontend.
decodeSession :: JSString -> Either String Session
decodeSession raw = eitherDecode (ByteString.pack (fromJSString raw))

-- | Run action with the decoded session, failing closed on invalid session JSON.
withSession :: JSString -> (Session -> IO JSString) -> IO JSString
withSession raw action = case decodeSession raw of
    Left msg ->
        jsonOut $
            Envelope [] (Failed (RuntimeFault (Error ("invalid session: " ++ msg)))) [] boot
    Right session -> action session

-- | Parse aspect name for 'resetSessionAPI'.
parseAspect :: String -> Maybe Aspect
parseAspect name = case name of
    "" -> Just All
    "all" -> Just All
    "vars" -> Just Vars
    "procs" -> Just Procs
    "break" -> Just Flag
    "trace" -> Just Trace
    _ -> Nothing

-- | Focus statement and its pretty-printed source for choice screens.
focusDetail :: Flight -> (Stm, String)
focusDetail fl = (fst (focus fl), prettify (fst (focus fl)))

-- | Response rendering: buffered events, step delta, status and updated session.
respond :: IORef [String] -> Configuration -> Session -> IO JSString
respond outputs conf session = do
    events <- reverse <$> readIORef outputs
    let stat = case (conf, flight session) of
            (Terminal state', _) -> Done state' (profile session)
            (Blocked suspension, Just fl) -> Awaiting suspension (Just (focusDetail fl)) False
            (Blocked suspension, Nothing) -> Awaiting suspension Nothing False
            (Stepped _, Just _) -> Running
            _ -> Failed (RuntimeFault (Error "unreachable engine state"))
    jsonOut (Envelope events stat (reverse (stepHistory session)) session)

-- | Fault response rendering: buffered events plus failed status and session.
-- Terminal faults drop the in-flight interpretation; the engine does not
-- return state changes of faulted computations, so the web side
-- re-establishes that invariant on the session handed back to the frontend.
failWith :: IORef [String] -> Fault -> Session -> IO JSString
failWith outputs fault session = do
    events <- reverse <$> readIORef outputs
    jsonOut (Envelope events (Failed fault) [] (flightless fault session))

-- | Session echo after a fault, dropping flights on terminal faults.
flightless :: Fault -> Session -> Session
flightless fault session = case fault of
    RuntimeFault _ -> session {flight = Nothing}
    Interrupted -> session {flight = Nothing}
    _ -> session

-- | Run one engine action with call-scoped output buffering and fresh step history.
perform :: Session -> Engine IO Configuration -> IO JSString
perform session action = do
    outputs <- newIORef []
    let zeroed = session {stepHistory = [], profile = emptyProfile}
    outcome <- runEngine (webHandlers outputs) zeroed action
    case outcome of
        Right (conf, session') -> respond outputs conf session'
        Left fault -> failWith outputs fault zeroed

-- | Immediate response without running the engine.
respondNow :: Session -> Status -> [String] -> IO JSString
respondNow session stat events = jsonOut (Envelope events stat [] session)

-- | Unused entrypoint: the JavaScript host or wasmtime drives the exported API.
main :: IO ()
main = return ()

-- | Export startup API.
foreign export javascript "start" startAPI :: IO ()

-- | Naive stdio REPL for wasmtime and friends, metacommands besides the listed bail.
startAPI :: IO ()
startAPI = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    loop boot
    where
        loop session = do
            putStr "IMP> "
            hFlush stdout
            done <- isEOF
            if done
                then return ()
                else do
                    line <- getLine
                    next <- runLine session line
                    case next of
                        Nothing -> return ()
                        Just session' -> loop session'
        runLine session line = case line of
            "" -> return (Just session)
            ":)" -> putStrLn "You look good today!" >> return (Just session) -- it's true
            ":quit" -> return Nothing
            ":help" -> putStrLn webHelp >> return (Just session)
            ":version" -> putStrLn versionText >> return (Just session)
            ":reset" -> do
                outcome <- runEngine stdioHandlers session (resetAspect All)
                case outcome of
                    Left fault -> print fault >> return (Just session)
                    Right (_, session') -> putStrLn "environment reset" >> return (Just session')
            ":show" -> putStrLn (renderAspect session All) >> return (Just session)
            _ ->
                case parser "stdio" line of
                    Left e -> print (ParseFail (unlines [line, show e])) >> return (Just session)
                    Right construct -> case construct of
                        Statement stm -> do
                            outcome <- runEngine stdioHandlers session (drive Natural stm)
                            case outcome of
                                Left fault -> print fault >> return (Just session)
                                Right (_, session') -> return (Just session')
                        Arithmetic aexp -> print (evaluate aexp (environment session)) >> return (Just session)
                        Boolean bexp ->
                            putStrLn (if evaluate bexp (environment session) then "true" else "false")
                                >> return (Just session)
                        Whitespace -> return (Just session)

-- | Export initial session API.
foreign export javascript "bootSession" bootSessionAPI :: IO JSString

-- | Initial empty session as JSON.
bootSessionAPI :: IO JSString
bootSessionAPI = jsonOut boot

-- | Export line evaluation API.
foreign export javascript "evaluate" evaluateAPI :: JSString -> JSString -> JSString -> IO JSString

-- | Evaluate one line of IMP source in the given session with the given step
-- budget, returning a protocol envelope with the updated session.
evaluateAPI :: JSString -> JSString -> JSString -> IO JSString
evaluateAPI rawSession rawLine rawFuel = withSession rawSession $ \session -> do
    let line = fromJSString rawLine
    let fuel = parseFuel rawFuel
    case parser "web" line of
        Left e ->
            respondNow session (Failed (RuntimeFault (ParseFail (unlines [line, show e])))) []
        Right construct -> case construct of
            Statement stm -> perform session (beginFueled fuel stm)
            Arithmetic aexp ->
                let value = show (evaluate aexp (environment session))
                in respondNow session (Done (environment session) emptyProfile) [value]
            Boolean bexp ->
                let value = if evaluate bexp (environment session) then "true" else "false"
                in respondNow session (Done (environment session) emptyProfile) [value]
            Whitespace -> respondNow session (Done (environment session) emptyProfile) []

-- | Export continuation API.
foreign export javascript "advance" advanceAPI :: JSString -> JSString -> IO JSString

-- | Continue the in-flight interpretation of the given session by the given fuel.
advanceAPI :: JSString -> JSString -> IO JSString
advanceAPI rawSession rawFuel = withSession rawSession $ \session ->
    perform session (advance (parseFuel rawFuel))

-- | Export input resolution API.
foreign export javascript "resolveInput" resolveInputAPI :: JSString -> JSString -> JSString -> IO JSString

-- | Resolve the current @read@ suspension with the given integer text, retrying on junk.
resolveInputAPI :: JSString -> JSString -> JSString -> IO JSString
resolveInputAPI rawSession rawInput rawFuel = withSession rawSession $ \session ->
    case readMaybe (fromJSString rawInput) of
        Just value ->
            perform session (proceedFueled (ResolveInput value) (parseFuel rawFuel))
        Nothing -> retryWith session

-- | Export choice resolution API.
foreign export javascript "resolveChoice" resolveChoiceAPI :: JSString -> JSString -> JSString -> IO JSString

-- | Resolve the current choice suspension with the given side @"true"@ for left.
resolveChoiceAPI :: JSString -> JSString -> JSString -> IO JSString
resolveChoiceAPI rawSession rawSide rawFuel = withSession rawSession $ \session ->
    perform session (proceedFueled (ResolveChoice (fromJSString rawSide == "true")) (parseFuel rawFuel))

-- | Export havoc resolution API.
foreign export javascript "resolveHavoc" resolveHavocAPI :: JSString -> JSString -> JSString -> IO JSString

-- | Resolve the current @havoc@ suspension with the given integer text.
resolveHavocAPI :: JSString -> JSString -> JSString -> IO JSString
resolveHavocAPI rawSession rawValue rawFuel = withSession rawSession $ \session ->
    case readMaybe (fromJSString rawValue) of
        Just value ->
            perform session (proceedFueled (ResolveHavoc value) (parseFuel rawFuel))
        Nothing -> retryWith session

-- | Repeat the current suspension with retry flag set, for invalid resolutions.
retryWith :: Session -> IO JSString
retryWith session = case flight session of
    Just fl
        | Blocked suspension <- blocked fl ->
            respondNow session (Awaiting suspension (Just (focusDetail fl)) True) []
    _ -> respondNow session (Failed NoFlight) []

-- | Export cancellation API.
foreign export javascript "cancel" cancelAPI :: JSString -> IO JSString

-- | Cancel the suspended interpretation of the given session.
cancelAPI :: JSString -> IO JSString
cancelAPI rawSession = withSession rawSession $ \session ->
    jsonOut (session {flight = Nothing})

-- | Export prompt query API.
foreign export javascript "getPrompt" getPromptAPI :: IO JSString

-- | Current prompt including separator and trailing space.
getPromptAPI :: IO JSString
getPromptAPI = return (toJSString "IMP> ")

-- | Export help text API.
foreign export javascript "helpText" helpTextAPI :: IO JSString

-- | Help text for the metacommands handled by the frontend.
helpTextAPI :: IO JSString
helpTextAPI = return (toJSString webHelp)

-- | Export version text API.
foreign export javascript "versionText" versionTextAPI :: IO JSString

-- | Version text of the interpreter.
versionTextAPI :: IO JSString
versionTextAPI = return (toJSString versionText)

-- | Export AST inspection API.
foreign export javascript "parseAst" parseAstAPI :: JSString -> IO JSString

-- | Parse IMP source and return its abstract syntax tree as JSON.
parseAstAPI :: JSString -> IO JSString
parseAstAPI input = do
    let line = fromJSString input
    case parser "ast" line :: Either ParseError Construct of
        Left e -> jsonOut (object ["tag" .= ("error" :: String), "message" .= show e])
        Right construct -> jsonOut (object ["tag" .= ("ok" :: String), "construct" .= construct])

-- | Export derivation tree API.
foreign export javascript "deriveTree" deriveTreeAPI :: JSString -> JSString -> IO JSString

-- | Derive the natural-semantics proof tree of the trace entry addressed by
-- its one-based index (newest first), exactly replaying the resolutions the
-- interpretation recorded. The response carries the tree and its output.
deriveTreeAPI :: JSString -> JSString -> IO JSString
deriveTreeAPI rawSession rawIndex = withSession rawSession $ \session ->
    let entries = trace session
    in case readMaybe (fromJSString rawIndex) of
        Just index
            | index >= 1 && index <= length entries -> do
                let
                    (stm, initial', recorded) = entries !! (index - 1)
                    replay =
                        Derive.Derivation
                            { Derive.stdin = [value | ResolveInput value <- recorded]
                            , Derive.havocs = [value | ResolveHavoc value <- recorded]
                            , Derive.choices = [left | ResolveChoice left <- recorded]
                            , Derive.stdout = []
                            }
                outcome <- runExceptT $ Derive.derive replay (stm, initial')
                case outcome of
                    Left exception ->
                        jsonOut (object ["tag" .= ("error" :: String), "message" .= show exception])
                    Right (derived, tree) ->
                        jsonOut $
                            object
                                [ "tag" .= ("tree" :: String)
                                , "tree" .= tree
                                , "stdout" .= Derive.stdout derived
                                ]
            | otherwise ->
                jsonOut (object ["tag" .= ("error" :: String), "message" .= ("index out of bounds: " ++ fromJSString rawIndex)])
        Nothing ->
            jsonOut (object ["tag" .= ("error" :: String), "message" .= ("invalid trace index: " ++ fromJSString rawIndex)])

-- | Export session reset API.
foreign export javascript "resetSession" resetSessionAPI :: JSString -> JSString -> IO JSString

-- | Reset the named aspect (or all on empty string) of the given session.
resetSessionAPI :: JSString -> JSString -> IO JSString
resetSessionAPI rawSession rawAspect = withSession rawSession $ \session ->
    case parseAspect (fromJSString rawAspect) of
        Nothing ->
            jsonOut $
                object
                    [ "tag" .= ("error" :: String)
                    , "message" .= ("unknown aspect: " ++ fromJSString rawAspect)
                    ]
        Just aspect -> do
            outputs <- newIORef []
            outcome <- runEngine (webHandlers outputs) session (resetAspect aspect)
            case outcome of
                Left fault -> failWith outputs fault session
                Right (_, session') ->
                    jsonOut (object ["tag" .= ("ok" :: String), "session" .= session'])
