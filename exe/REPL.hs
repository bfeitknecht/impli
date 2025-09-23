{-# LANGUAGE TypeApplications #-}

{- |
Module      : REPL
Description : Read-Evaluate-Print-Loop for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides the Read-Evaluate-Print-Loop for the IMP language interpreter.
This includes interactive parsing into 'IMP.Syntax.Construct' followed by interpretation
with 'IMP.Expression.evaluate' or 'IMP.Statement.execute'.
Supports various metacommands, such as inspection of interpreter state, interpret source file,
print AST of IMP language construct and save execution history to disk.
-}
module REPL where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import System.Console.Haskeline hiding (display)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import Config
import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax

-- | Encapsulation of computation in 'IMP.REPL'.
-- type REPL = StateT Store (InputT IMP)
type REPL = StateT Store (ExceptT Exception (InputT IO))

-- | Encapsulation of REPL customization.
data Setup = Setup
    { settings :: Settings IO
    , prefs :: Prefs -- INFO: for more information visit https://github.com/haskell/haskeline/wiki/UserPreferences
    }

defaultSetup :: Setup
defaultSetup = Setup {settings = defaultSettings, prefs = defaultPrefs}

-- | Setup with arguments.
setup :: Maybe FilePath -> Maybe FilePath -> IO Setup
setup Nothing Nothing = return defaultSetup
setup hist conf = do
    let settings' = defaultSettings {historyFile = hist}
    prefs' <- maybe (return defaultPrefs) readPrefs conf
    return Setup {settings = settings', prefs = prefs'}

-- | Modifiable options in 'repl' through @:set@ and @:unset@.
data Option
    = Welcome String
    | Prompt String
    | Goodbye String
    | Verbose Int -- INFO: non-negative integer!
    -- 0: normal
    -- 1: info (time, space)
    -- 2: debug (parse, execution, ...)
    deriving (Eq, Ord, Show)

-- | Default options.
defaults :: [Option]
defaults =
    [ Welcome welcome
    , Prompt prompt
    , Goodbye goodbye
    , Verbose 1
    ]

-- | Encapsulation of state in 'IMP.REPL'.
data Store = Store
    { _state :: State
    , _trace :: [Stm]
    , _defaults :: [Option]
    , _welcome :: String
    , _prompt :: String
    , _goodbye :: String
    , _verbose :: Int
    , _multiline :: Maybe Int
    }

-- | Starting data store for 'repl'.
start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , _defaults = defaults
        , _welcome = welcome
        , _prompt = prompt
        , _goodbye = goodbye
        , _verbose = 0
        , _multiline = Nothing
        }

-- | Read-Evaluate-Print-Loop function in the 'REPL' monad.
repl :: Setup -> Store -> IO ()
repl (Setup s p) store = do
    putStrLn $ _welcome store
    runInputTWithPrefs p s (runExceptT (execStateT loop store))
        >>= either (\e -> print e >> exitFailure) (\_ -> putStrLn goodbye)

-- | REPL loop that processes input and maintains interpreter state.
loop :: REPL ()
loop = handleInterrupt loop $ do
    prompt' <- gets _prompt
    multi <- gets _multiline
    line <- lift . lift . getInputLine $ case multi of
        Nothing -> prompt' ++ "> "
        Just i -> replicate (length prompt') '.' ++ replicate i '>' ++ " "
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just "" -> loop -- empty line, loop
        Just (':' : rest) -> handleMeta . normalizeMeta $ words rest
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch c >> loop)
                (parser "interactive" input)
        `catchError` \e -> case e of
            Empty -> display Whitespace -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- irrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop -- mistakes happen

-- | Lift computation from 'IMP.State.IMP' into 'REPL'.
liftIMP :: IMP a -> REPL a
liftIMP m = (lift . liftIO . runExceptT) m >>= either throwError return

-- | Process construct in environment, return updated environment.
dispatch :: Construct -> REPL ()
dispatch construct = do
    trace <- gets _trace
    state <- gets _state
    case construct of
        Statement stm -> handleInterrupt loop $ do
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state', _trace = stm : trace}
        Arithmetic aexp -> display (evaluate state aexp)
        Boolean bexp -> outputln (if evaluate state bexp then "true" else "false")
        Whitespace -> return ()

-- | Help message displayed when user enters @:help@ metacommand.
helpMessage :: [String]
helpMessage =
    [ ":help / :?               Show this help message"
    , ":quit                    Quit REPL"
    , ":clear                   Clear screen"
    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
    , ":trace                   Show trace (executed statements)"
    , ":state                   Show state definitions (variables, procedures, break flag)"
    , ":load FILE               Interpret file and load resulting state"
    , ":write FILE              Write trace to file (relative to $PWD)"
    , ":ast (INPUT | #n)        Parse and display AST of input or n-th statement in trace"
    ]

-- | Expand metacommand abbreviations.
normalizeMeta :: [String] -> [String]
normalizeMeta ["?"] = ["help"]
normalizeMeta ["h"] = ["help"]
normalizeMeta ["q"] = ["quit"]
normalizeMeta ["c"] = ["clear"]
normalizeMeta ["t"] = ["trace"]
normalizeMeta ["s"] = ["state"]
normalizeMeta (w : ws)
    | w `elem` ["l", "load"] = ["load", it]
    | w `elem` ["w", "write"] = ["write", it]
    | w `elem` ["a", "ast"] = ["ast", it]
    | w `elem` ["r", "reset"] = ["reset", aspect]
    where
        it = unwords ws
        aspect
            | it `elem` ["v", "vars", "variables"] = "vars"
            | it `elem` ["p", "procs", "procedures"] = "procs"
            | it `elem` ["b", "break"] = "break"
            | it `elem` ["t", "trace"] = "trace"
            | otherwise = it
normalizeMeta rest = rest

-- | Process metacommand in environment, continue loop or exit.
handleMeta :: [String] -> REPL ()
handleMeta meta = case meta of
    [")"] -> outputln "You look good today!" >> loop
    ["help"] -> do
        explain
            "All meta commands can be abbreviated by their first letter."
            helpMessage
        loop
    ["quit"] -> return ()
    ["clear"] -> clear >> loop
    ["reset", it] -> reset it
    ["trace"] -> do
        -- CHECK: is there some better way to do this without reverse?
        trace <- gets _trace
        explain
            "Trace:"
            [ init . unlines $ zipWith (++) (indx : bufs) (lines s)
            | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
            , let
                indx = '#' : show i ++ space 2
                bufs = repeat . space $ length (show i) + 3
            ]
        loop
    ["state"] -> do
        (vars, procs, flag) <- gets _state
        explain
            "Variables:"
            -- INFO: invariant of IMP.State.setVar guarantees no empty string key
            [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        explain "Procedures:" [prettify p | p <- procs]
        outputln $ "Break: " ++ show flag ++ "\n"
        loop
    ["load", path]
        | null path -> throwError . Info $ "no filepath provided"
        | otherwise -> loadIMP path >> loop
    ["write", path]
        | null path -> throwError . Info $ "no filepath provided"
        | otherwise -> writeIMP path >> loop
    ["ast", it] -> ast it
    _ ->
        throwError . Error $
            unlines
                [ "not a meta command: :" ++ unwords meta
                , "Enter :help to list available metacommands and :quit to exit."
                ]

-- | Reset specific aspect of the environment.
reset :: String -> REPL ()
reset it = do
    state <- gets _state
    case it of
        "" -> modify (\st -> st {_state = initial, _trace = []}) >> (throwError . Info) "environment reset"
        "vars" -> modify (\st -> st {_state = resetVars state}) >> (throwError . Info) "variables reset"
        "procs" -> modify (\st -> st {_state = resetProcs state}) >> (throwError . Info) "procedures reset"
        "break" -> modify (\st -> st {_state = resetBreak state}) >> (throwError . Info) "break flag reset"
        "trace" -> modify (\st -> st {_trace = []}) >> (throwError . Info) "trace reset"
        _ -> throwError . Error $ "unrecognized aspect to reset: " ++ it

-- | Print AST of specific construct.
ast :: String -> REPL ()
ast "" = throwError . Info $ "nothing to parse"
ast "#" = throwError . Info $ "no index provided"
ast ('#' : ds) = case readMaybe ds of
    Nothing -> throwError . ParseFail $ unwords ["index", '#' : ds]
    Just i -> do
        trace <- gets _trace
        if i <= 0 || i > length trace
            then throwError . Error $ "index out of bounds: " ++ show i
            -- INFO: condition guarantees index in bounds
            else display (trace !! (length trace - i)) >> loop
ast it = liftIO (printAST it) >> loop

-- | Interpret IMP language source file, return updated state.
loadIMP :: FilePath -> REPL ()
loadIMP path = do
    content <-
        liftIO (readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state <- gets _state
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state'}
            throwError . Info $ "interpreted: " ++ path

-- | Write trace to specified file.
writeIMP :: FilePath -> REPL ()
writeIMP path = do
    content <- gets (prettytrace . _trace)
    _ <-
        liftIO (writeFile path content)
            `catchError` (\e -> throwError . IOFail $ unlines ["write trace to: " ++ path, show e])
    throwError . Info $ "wrote trace to: " ++ path

-- | Parse input and print AST.
printAST :: String -> IO ()
printAST input =
    either
        (\e -> print . ParseFail $ unlines [input, show e])
        print
        (parser @Construct "AST" input)

-- | Convert trace to valid IMP language source code.
prettytrace :: [Stm] -> String
prettytrace = prettify . mconcat -- Haskell is nice!

-- | Nicely format 'outputln' with heading and indented body.
explain :: String -> [String] -> REPL ()
explain heading [] = outputln heading
explain heading body = outputln $ heading ++ '\n' : indent 4 (unlines body)

-- | Clear the terminal.
clear :: REPL ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
