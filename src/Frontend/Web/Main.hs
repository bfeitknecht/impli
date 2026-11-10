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

Exposes an export-only WASM interface over 'IMP.Engine': no foreign imports.
The JavaScript frontend drives prompt, input and metacommands, calls
@evaluate@ with lines of IMP source and continues suspended interpretation
through the @resolve@ exports. All results are JSON protocol envelopes.

The @start@ export runs a naive stdio REPL usable in wasmtime and friends,
handling only plain statements and a minimal set of metacommands.
-}

module Main where

import Data.Aeson
import Data.IORef
import Data.Version (showVersion)
import GHC.Wasm.Prim
import System.IO
import System.IO.Unsafe (unsafePerformIO)
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

import qualified Paths_impli as Paths

-- | Session state shared across API calls.
{-# NOINLINE sessionRef #-}
sessionRef :: IORef Session
sessionRef = unsafePerformIO (newIORef boot)

-- | Output lines buffered during the current API call.
{-# NOINLINE outputsRef #-}
outputsRef :: IORef [String]
outputsRef = unsafePerformIO (newIORef [])

-- | Interrupt flag polled between kernel steps.
{-# NOINLINE interruptRef #-}
interruptRef :: IORef Bool
interruptRef = unsafePerformIO (newIORef False)

-- | Handlers over IO for explicit browser driving.
-- Suspensions are never discharged here: interpretation pauses at blocked
-- configurations and the frontend continues via the resolution exports.
webHandlers :: Handlers IO
webHandlers =
    Handlers
        { onInput = \_ -> error "browser discharges input suspensions via resolveInput"
        , onOutput = \text -> modifyIORef outputsRef (text :)
        , onChoice = error "browser discharges choice suspensions via resolveChoice"
        , onRandom = error "browser discharges havoc suspensions via resolveHavoc"
        , pollInterrupt = readIORef interruptRef
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

-- | Encode value as JSON across the FFI boundary.
jsonOut :: (ToJSON a) => a -> IO JSString
jsonOut = return . toJSString . ByteString.unpack . encode

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

-- | Shared response rendering: buffered events plus status from configuration.
respond :: Configuration -> Session -> IO JSString
respond conf session = do
    events <- reverse <$> readIORef outputsRef
    let stat = case (conf, flight session) of
            (Terminal state', _) -> Done state' (profile session)
            (Blocked suspension, Just fl) -> Awaiting suspension (focusOf fl) False
            (Blocked suspension, Nothing) -> Awaiting suspension Nothing False
            _ -> Failed (RuntimeFault (Error "unreachable engine state"))
    jsonOut (Envelope events stat)
    where
        focusOf = Just . focusDetail

-- | Focus statement and its pretty-printed source for choice screens.
focusDetail :: Flight -> (Stm, String)
focusDetail fl = (fst (focus fl), prettify (fst (focus fl)))

-- | Fault response rendering: buffered events plus failed status.
failWith :: Fault -> IO JSString
failWith fault = do
    events <- reverse <$> readIORef outputsRef
    jsonOut (Envelope events (Failed fault))

-- | Begin structural interpretation of statement in session.
runStatement :: Session -> Stm -> IO JSString
runStatement session stm = do
    outcome <- runEngine webHandlers session (begin stm)
    case outcome of
        Left fault -> failWith fault
        Right (conf, session') -> do
            writeIORef sessionRef session'
            respond conf session'

-- | Resolve current suspension and continue interpretation.
continueWith :: Resolution -> IO JSString
continueWith resolution = do
    writeIORef outputsRef []
    session <- readIORef sessionRef
    outcome <- runEngine webHandlers session (proceed resolution)
    case outcome of
        Left fault -> failWith fault
        Right (conf, session') -> do
            writeIORef sessionRef session'
            respond conf session'

-- | Repeat the current suspension with retry flag set, for invalid resolutions.
retrySuspension :: IO JSString
retrySuspension = do
    session <- readIORef sessionRef
    case flight session of
        Just fl
            | Blocked suspension <- blocked fl ->
                jsonOut (Envelope [] (Awaiting suspension (Just (focusDetail fl)) True))
        _ -> jsonOut (Envelope [] (Failed NoFlight))

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

-- | Export line evaluation API.
foreign export javascript "evaluate" evaluateAPI :: JSString -> IO JSString

-- | Evaluate one line of IMP source in the current session, return protocol envelope.
evaluateAPI :: JSString -> IO JSString
evaluateAPI input = do
    let line = fromJSString input
    writeIORef interruptRef False
    writeIORef outputsRef []
    session <- readIORef sessionRef
    case parser "web" line of
        Left e -> jsonOut (Envelope [] (Failed (RuntimeFault (ParseFail (unlines [line, show e])))))
        Right construct -> case construct of
            Statement stm -> runStatement session stm
            Arithmetic aexp ->
                let value = show (evaluate aexp (environment session))
                in jsonOut (Envelope [value] (Done (environment session) emptyProfile))
            Boolean bexp ->
                let value = if evaluate bexp (environment session) then "true" else "false"
                in jsonOut (Envelope [value] (Done (environment session) emptyProfile))
            Whitespace -> jsonOut (Envelope [] (Done (environment session) emptyProfile))

-- | Export input resolution API.
foreign export javascript "resolveInput" resolveInputAPI :: JSString -> IO JSString

-- | Resolve the current @read@ suspension with the given integer text, retrying on junk.
resolveInputAPI :: JSString -> IO JSString
resolveInputAPI input =
    case readMaybe (fromJSString input) of
        Just value -> continueWith (ResolveInput value)
        Nothing -> retrySuspension

-- | Export choice resolution API.
foreign export javascript "resolveChoice" resolveChoiceAPI :: JSString -> IO JSString

-- | Resolve the current choice suspension with the given side @\"true\"@ for left.
resolveChoiceAPI :: JSString -> IO JSString
resolveChoiceAPI = continueWith . ResolveChoice . (== "true") . fromJSString

-- | Export havoc resolution API.
foreign export javascript "resolveHavoc" resolveHavocAPI :: JSString -> IO JSString

-- | Resolve the current @havoc@ suspension with the given integer text.
resolveHavocAPI :: JSString -> IO JSString
resolveHavocAPI input =
    case readMaybe (fromJSString input) of
        Just value -> continueWith (ResolveHavoc value)
        Nothing -> retrySuspension

-- | Export interrupt API.
foreign export javascript "interrupt" interruptAPI :: IO ()

-- | Interrupt running interpretation and cancel any suspended interpretation.
interruptAPI :: IO ()
interruptAPI = do
    writeIORef interruptRef True
    modifyIORef sessionRef $ \session -> session {flight = Nothing}

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

-- | Export state inspection API.
foreign export javascript "showState" showStateAPI :: IO JSString

-- | Current session environment as JSON.
showStateAPI :: IO JSString
showStateAPI = readIORef sessionRef >>= jsonOut . environment

-- | Export trace inspection API.
foreign export javascript "showTrace" showTraceAPI :: IO JSString

-- | Current session trace as JSON, newest statement first with one-based index.
showTraceAPI :: IO JSString
showTraceAPI = do
    session <- readIORef sessionRef
    jsonOut [TraceEntry n (prettify stm) stm | (n, stm) <- zip [1 ..] (trace session)]

-- | Export step history inspection API.
foreign export javascript "showSteps" showStepsAPI :: IO JSString

-- | Steps of the latest interpretation in chronological order with resource profile.
showStepsAPI :: IO JSString
showStepsAPI = do
    session <- readIORef sessionRef
    jsonOut (Steps (reverse (stepHistory session)) (profile session))

-- | Export AST inspection API.
foreign export javascript "parseAst" parseAstAPI :: JSString -> IO JSString

-- | Parse IMP source and return its abstract syntax tree as JSON.
parseAstAPI :: JSString -> IO JSString
parseAstAPI input = do
    let line = fromJSString input
    case parser "ast" line :: Either ParseError Construct of
        Left e -> jsonOut (object ["tag" .= ("error" :: String), "message" .= show e])
        Right construct -> jsonOut (object ["tag" .= ("ok" :: String), "construct" .= construct])

-- | Export session reset API.
foreign export javascript "resetSession" resetSessionAPI :: IO JSString

-- | Reset the named aspect (or all on empty string) of the session environment.
resetSessionAPI :: JSString -> IO JSString
resetSessionAPI input = do
    session <- readIORef sessionRef
    case parseAspect (fromJSString input) of
        Nothing ->
            jsonOut $
                object
                    [ "tag" .= ("error" :: String)
                    , "message" .= ("unknown aspect: " ++ fromJSString input)
                    ]
        Just aspect -> do
            outcome <- runEngine webHandlers session (resetAspect aspect)
            case outcome of
                Left fault -> failWith fault
                Right (_, session') -> do
                    writeIORef sessionRef session'
                    jsonOut (object ["tag" .= ("ok" :: String)])
