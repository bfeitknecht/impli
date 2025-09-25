{-# LANGUAGE ConstrainedClassMethods #-}
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

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import Meta
import Preset

-- | Encapsulation of computation in 'IMP.REPL'.
type REPL = StateT Store (ExceptT Exception (InputT IO))

-- | Encapsulation of REPL customization.
data Setup = Setup
    { settings :: Settings IO
    , prefs :: Prefs -- INFO: for more information visit https://github.com/haskell/haskeline/wiki/UserPreferences
    }

-- | Default REPL customization.
defaultSetup :: Setup
defaultSetup = Setup {settings = defaultSettings, prefs = defaultPrefs}

-- | Setup with arguments.
setup :: Maybe FilePath -> Maybe FilePath -> IO Setup
setup Nothing Nothing = return defaultSetup
setup hist conf = do
    let settings' = defaultSettings {historyFile = hist}
    prefs' <- maybe (return defaultPrefs) readPrefs conf
    return Setup {settings = settings', prefs = prefs'}

-- | Encapsulation of state in 'IMP.REPL'.
data Store = Store
    { _state :: State
    , _trace :: [Stm]
    , _defaults :: Defaults
    , _welcome :: String
    , _prompt :: String
    , _separator :: Char
    , _goodbye :: String
    , _verbose :: Level
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
        , _separator = separator
        , _goodbye = goodbye
        , _verbose = verbosity
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
    separator' <- gets _separator
    line <- lift . lift . getInputLine $ prompt' ++ separator' : " "
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just ":)" -> outputln "You look good today!" >> loop -- it's true
        Just (':' : meta) ->
            either
                (const . throwError . Error $ unlines ["not a meta command: :" ++ meta, hint])
                (dispatch @Command)
                (parser "meta" meta)
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch @Construct c >> loop)
                (parser "interactive" input)
        `catchError` \e -> case e of
            Empty -> display Whitespace -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- irrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop -- mistakes happen

-- | Lift computation from 'IMP.State.IMP' into 'REPL'.
liftIMP :: IMP a -> REPL a
liftIMP m = (lift . liftIO . runExceptT) m >>= either throwError return

-- | TODO
class Dispatches a where
    -- | TODO
    dispatch :: (Parses a) => a -> REPL ()

-- | TODO
instance Dispatches Construct where
    -- \| TODO
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

-- | TODO
instance Dispatches Command where
    -- \| TODO
    dispatch Quit = return ()
    dispatch command =
        case command of
            Help -> explain "All metacommands unrelated to settings can be abbreviated by their first letter" helpMessage
            Clear -> clear
            Reset aspect -> reset aspect
            Show aspect -> shower aspect
            Load path -> loadIMP path
            Write path -> writeIMP path
            AST element -> ast element
            Set option -> set option
            >> loop

-- | TODO
reset :: Aspect -> REPL ()
reset aspect = do
    state <- gets _state
    case aspect of
        All -> modify (\st -> st {_state = initial, _trace = []}) >> (throwError . Info) "environment reset"
        Vars -> modify (\st -> st {_state = resetVars state}) >> (throwError . Info) "variables reset"
        Procs -> modify (\st -> st {_state = resetProcs state}) >> (throwError . Info) "procedures reset"
        Flag -> modify (\st -> st {_state = resetBreak state}) >> (throwError . Info) "break flag reset"
        Trace -> modify (\st -> st {_trace = []}) >> (throwError . Info) "trace reset"

-- | TODO
shower :: Aspect -> REPL ()
shower aspect = do
    (vars, procs, flag) <- gets _state
    trace <- gets _trace
    case aspect of
        All -> shower Vars >> shower Procs >> shower Flag >> shower Trace
        Vars ->
            explain
                "Variables:"
                -- INFO: invariant of IMP.State.setVar guarantees no empty string key
                [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        Procs -> explain "Procedures:" [prettify p | p <- procs]
        Flag -> outputln $ "Break: " ++ show flag ++ "\n"
        Trace ->
            explain
                "Trace:"
                [ init . unlines $ zipWith (++) (indx : bufs) (lines s)
                | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
                , let
                    indx = '#' : show i ++ space 2
                    bufs = repeat . space $ length (show i) + 3
                ]

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

-- | TODO
ast :: Element -> REPL ()
ast (Input construct) = display construct
ast (Index n) = do
    trace <- gets _trace
    if n <= 0 || n > length trace
        then throwError . Error $ "index out of bounds: " ++ show n
        -- INFO: condition guarantees index in bounds
        else display (trace !! (length trace - n))

-- | TODO
set :: Option -> REPL ()
set option = case option of
    Welcome w -> modify $ \st -> st {_welcome = w}
    Prompt p -> modify $ \st -> st {_prompt = p}
    Separator s -> modify $ \st -> st {_separator = s}
    Goodbye g -> modify $ \st -> st {_goodbye = g}
    Verbose v -> modify $ \st -> st {_verbose = v}

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
