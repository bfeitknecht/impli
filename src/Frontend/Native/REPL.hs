{- |
Module      : Frontend.Native.REPL
Description : Read-Evaluate-Print-Loop for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the Read-Evaluate-Print-Loop for the IMP language interpreter.
This includes interactive parsing into 'IMP.Syntax.Construct' followed by
interpretation through 'IMP.Engine.drive', discharging kernel effects with
haskeline-based handlers.

Supports metacommands, such as inspection of interpreter state, interpretation
of source files, printing the AST of IMP language constructs and saving
execution history to disk. Verbosity levels change the prompt separator and
report resource usage: @profile@ shows elapsed time and steps taken, @debug@
additionally shows the parsed AST and intermediate configurations.
-}
module Frontend.Native.REPL (
    Options (..),
    defaultOptions,
    repl,
    interpret,
) where

import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad (when)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)
import System.CPUTime (getCPUTime)
import System.Console.Haskeline hiding (display)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError)
import System.Random (randomIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Control.Exception as Exception
import qualified System.Console.ANSI as ANSI

import Frontend.Native.Meta
import IMP.Engine hiding (Profile)
import IMP.Exception (Exception (..))
import IMP.Expression
import IMP.Parser
import IMP.Syntax

import qualified Paths_impli as Paths

-- | Type alias for haskeline-based IO.
type IO' = InputT IO

-- | Options of the native frontend.
data Options = Options
    { optLevel :: Level
    -- ^ Verbosity level.
    , optSemantics :: Semantics
    -- ^ Semantics to interpret with.
    , optHistory :: Maybe FilePath
    -- ^ History file for haskeline.
    }

-- | Default options: normal verbosity, natural semantics, no history file.
defaultOptions :: Options
defaultOptions = Options {optLevel = Normal, optSemantics = Natural, optHistory = Nothing}

-- | Message displayed on entry of 'repl'.
welcome :: String
welcome = unwords ["Welcome to the IMP REPL!", hint]

-- | Hint displayed on entry of 'repl' or metacommand error.
hint :: String
hint = "Enter :help to list available metacommands or :quit to exit."

-- | Message displayed after clean exit.
goodbye :: String
goodbye = "Goodbye!"

-- | Prompt string for verbosity level.
prompt :: Level -> String
prompt level = "IMP" ++ separator level : " "

-- | Prompt separator for verbosity level.
separator :: Level -> Char
separator Normal = '>'
separator Profile = '+'
separator Debug = '*'

-- | Read a possibly multi-line construct, continuing as long as parentheses
-- or @end@-terminated blocks are unbalanced. Metacommands stay single-line.
readInput :: Level -> IO' (Maybe String)
readInput level = do
    line <- getInputLine (prompt level)
    case line of
        Nothing -> return Nothing
        Just input@(':' : _) -> return (Just input) -- metacommand, always single line
        Just input -> continue input
    where
        continue input
            | depthIn input <= 0 = return (Just input)
            | otherwise = do
                line <- getInputLine ("IMP" ++ '|' : " ")
                case line of
                    Nothing -> return Nothing -- ctrl-d during continuation, abort input
                    Just more -> continue (input ++ "\n" ++ more)

-- | Balance score: parentheses plus end-terminated block keywords.
depthIn :: String -> Int
depthIn input = sum (map paren input) + sum (map blocks (words input))
    where
        paren '(' = 1
        paren ')' = -1
        paren _ = 0
        blocks word
            | word `elem` openings = 1
            | word == "end" = -1
            | otherwise = 0
        openings =
            [ "if"
            , "while"
            , "do"
            , "for"
            , "repeat"
            , "let"
            , "procedure"
            , "revert"
            , "match"
            , "flip"
            , "try"
            , "timeout"
            ]

-- | Exception thrown on read abort, caught for clean exit.
data Aborted = Aborted
    deriving (Eq, Show)

instance Exception.Exception Aborted

-- | Loop state of the REPL.
data Loop = Loop
    { options :: Options
    , session :: Session
    }

-- | Handlers over haskeline base monad for interactive interpretation.
tHandlers :: Handlers IO'
tHandlers =
    Handlers
        { onInput = askFor
        , onOutput = outputStrLn
        , onChoice = liftIO randomIO
        , onRandom = liftIO randomIO
        , pollInterrupt = return False
        }
    where
        askFor x = do
            line <- getInputLine (x ++ " := ")
            case line of
                Nothing -> liftIO (Exception.throwIO Aborted) -- ctrl-d during read, exit cleanly
                Just input ->
                    case readMaybe input of
                        Nothing -> inform "invalid input, please enter an integer" >> askFor x
                        Just i -> return i

-- | Handlers over standard IO for non-interactive interpretation.
stdioHandlers :: Handlers IO
stdioHandlers =
    Handlers
        { onInput = askFor
        , onOutput = putStrLn
        , onChoice = randomIO
        , onRandom = randomIO
        , pollInterrupt = return False
        }
    where
        askFor x = do
            putStr (x ++ " := ") >> hFlush stdout
            result <- Exception.try getLine
            case result of
                Left e
                    | isEOFError e -> exitSuccess -- ctrl-d during read, exit cleanly
                    | otherwise -> Exception.throwIO e
                Right input ->
                    case readMaybe input of
                        Nothing -> putStrLn (show (Info "invalid input, please enter an integer")) >> askFor x
                        Just i -> return i

-- | Read-Evaluate-Print-Loop with haskeline for interactive use.
repl :: Options -> IO ()
repl opts = do
    putStrLn welcome
    let settings = defaultSettings {historyFile = optHistory opts}
    outcome <- runInputT settings $ withInterrupt $ try (loop (Loop opts boot))
    case outcome of
        Left Aborted -> putStrLn goodbye -- ctrl-d during read, flush line and exit cleanly
        Right () -> putStrLn goodbye -- ctrl-d or :quit, clean exit

-- | REPL loop that processes input and maintains interpreter state.
loop :: Loop -> IO' ()
loop l = handleInterrupt (loop l) $ do
    result <- try (iteration l)
    case result of
        Left UserInterrupt -> outputStrLn "\n*** ERROR: interrupted" >> loop l
        Left e -> liftIO (Exception.throwIO e)
        Right Nothing -> return ()
        Right (Just l') -> loop l'

-- | One iteration of the REPL loop, return 'Nothing' to terminate.
iteration :: Loop -> IO' (Maybe Loop)
iteration l@(Loop options _) = do
    line <- readInput (optLevel options)
    case line of
        Nothing -> return Nothing -- ctrl-d, exit cleanly
        Just ":)" -> outputStrLn "You look good today!" >> return (Just l) -- it's true
        Just (':' : meta) ->
            case parser "meta" meta of
                Left _ -> do
                    outputStrLn $ unlines ["unrecognized meta command: :" ++ meta, hint]
                    return (Just l)
                Right command -> commandeer command l
        Just input ->
            case parser "interactive" input of
                Left e -> do
                    outputStrLn . show . ParseFail $ unlines [input, show e]
                    return (Just l)
                Right construct -> do
                    (l', _) <- dispatch construct l
                    return (Just l')

-- | Dispatch execution of construct, return updated loop state and whether it succeeded.
dispatch :: Construct -> Loop -> IO' (Loop, Bool)
dispatch construct l@(Loop options session) = case construct of
    Statement stm -> do
        let level = optLevel options
        let debug = level == Debug
        when debug $ outputStrLn ("AST: " ++ show stm)
        start <- liftIO getCPUTime
        outcome <- runEngine tHandlers session (drive (if debug then Structural else optSemantics options) stm)
        end <- liftIO getCPUTime
        case outcome of
            Left fault -> do
                report level (end - start) Nothing
                case fault of
                    RuntimeFault e@(AssertFail _) -> fatal e -- irrecoverable, propagate
                    RuntimeFault e@(Raised _) -> fatal e -- ''
                    _ -> outputStrLn (show fault) >> return (l, False) -- mistakes happen
            Right (_, session') -> do
                report level (end - start) (Just session')
                return (l {session = session'}, True)
    Arithmetic aexp -> do
        outputStrLn $ show (evaluate aexp (environment session))
        return (l, True)
    Boolean bexp -> do
        outputStrLn $ if evaluate bexp (environment session) then "true" else "false"
        return (l, True)
    Whitespace -> return (l, True)

-- | Execute metacommand, return 'Nothing' to terminate.
commandeer :: Command -> Loop -> IO' (Maybe Loop)
commandeer command l@(Loop options session) = case command of
    Quit -> return Nothing
    Help -> outputStrLn helpText >> return (Just l)
    Clear -> liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0) >> return (Just l)
    Version -> outputStrLn version >> return (Just l)
    Reset aspect ->
        runEngine tHandlers session (resetAspect aspect) >>= \outcome -> case outcome of
            Left fault -> outputStrLn (show fault) >> return (Just l)
            Right (_, session') -> inform (message aspect) >> return (Just l {session = session'})
    Show aspect -> outputStrLn (renderAspect session aspect) >> return (Just l)
    Load path -> do
        content <- liftIO $ Exception.try (readFile path)
        case content of
            Left e -> do
                outputStrLn . show . IOFail $ unlines ["read from: " ++ path, show (e :: Exception.IOException)]
                return (Just l)
            Right input ->
                case parser path input of
                    Left e -> do
                        outputStrLn . show . ParseFail $ unlines [path, show e]
                        return (Just l)
                    Right stm -> do
                        (l', success) <- dispatch (Statement stm) l
                        when success $ inform ("interpreted: " ++ path)
                        return (Just l')
    Write path -> do
        let content = traceSource session
        result <- liftIO $ Exception.try (writeFile path content)
        case result of
            Left e -> do
                outputStrLn . show . IOFail $ unlines ["write trace to: " ++ path, show (e :: Exception.IOException)]
                return (Just l)
            Right () -> inform ("wrote trace to: " ++ path) >> return (Just l)
    AST element ->
        case element of
            Input construct' -> outputStrLn (show construct') >> return (Just l)
            Index n -> do
                let executed = trace session
                if n <= 0 || n > length executed
                    then outputStrLn . show . Error $ "index out of bounds: " ++ show n
                    else outputStrLn . show $ executed !! (length executed - n)
                return (Just l)
    Verbose level -> inform ("verbosity: " ++ levelName level) >> return (Just l {options = options {optLevel = level}})
    where
        message aspect = case aspect of
            All -> "environment reset"
            Vars -> "variables reset"
            Procs -> "procedures reset"
            Flag -> "break flag reset"
            Trace -> "trace reset"

-- | Interpret IMP source non-interactively via channel name, exiting with failure on error.
interpret :: Semantics -> String -> String -> IO ()
interpret semantics channel input =
    case parser channel input of
        Left e -> print (ParseFail (unlines [input, show e])) >> exitFailure
        Right stm -> do
            outcome <- runEngine stdioHandlers boot (drive semantics stm)
            case outcome of
                Left fault -> print fault >> exitFailure
                Right _ -> return ()

-- | Report resource usage for non-normal verbosity levels.
report :: Level -> Integer -> Maybe Session -> IO' ()
report Normal _ _ = return ()
report level picos msession = do
    when (level == Debug) $ case msession of
        Just session' -> mapM_ (outputStrLn . renderStep) (reverse (stepHistory session'))
        Nothing -> return ()
    outputStrLn $ "[" ++ levelName level ++ "] elapsed: " ++ milliseconds ++ metrics msession
    where
        milliseconds = printf "%.3f" (fromIntegral picos / 1e9 :: Double) ++ " ms"
        metrics (Just session')
            | stepCount (profile session') > 0 = ", " ++ renderProfile (profile session')
        metrics _ = ""

-- | Nicely output information message.
inform :: String -> IO' ()
inform = outputStrLn . show . Info

-- | Output the version.
version :: String
version = unwords ["impli", showVersion Paths.version]

-- | Print exception and exit with failure, for irrecoverable faults.
fatal :: Exception -> IO' a
fatal e = liftIO (print e >> exitFailure)
