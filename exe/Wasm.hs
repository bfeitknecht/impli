{-# LANGUAGE TypeApplications #-}

{- |
Module      : Main
Description : WASM-compatible entrypoint for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides a WASM-compatible entrypoint that uses basic IO instead of haskeline.
This allows compilation to WASM/WASI without terminfo dependencies.
-}
module Main where

import Control.Monad.Except
import Control.Monad.State hiding (State, state)
import Data.Version (showVersion)
import System.IO (hFlush, isEOF, stdout)
import qualified System.Exit as Exit

import qualified Data.Map as Map
import qualified Paths_impli as Paths

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax
import REPL.Meta
import REPL.Preset

-- | Encapsulation of state in the simple REPL
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

-- | Starting store for the REPL
start :: Store
start =
    Store
        { _state = initial
        , _trace = []
        , _defaults = defaults
        , _welcome = welcome
        , _prompt = prompt
        , _separator = normalsep
        , _goodbye = goodbye
        , _verbose = verbosity
        }

-- | Simple REPL monad without haskeline dependency
type SimpleREPL = StateT Store (ExceptT Exception IO)

-- | Lift computation from 'IMP.State.IMP' into 'SimpleREPL'.
liftIMP :: IMP a -> SimpleREPL a
liftIMP m = (lift . liftIO . runExceptT) m >>= either throwError return

-- | Entrypoint for WASM-compatible IMP interpreter
main :: IO ()
main = do
    putStrLn (_welcome start)
    result <- runExceptT (execStateT loop start)
    case result of
        Left e -> print e >> Exit.exitFailure
        Right _ -> putStrLn goodbye

-- | Main REPL loop using simple IO
loop :: SimpleREPL ()
loop = do
    prompt' <- gets _prompt
    separator' <- gets _separator
    liftIO $ putStr (prompt' ++ separator' : " ") >> hFlush stdout
    
    eof <- liftIO isEOF
    if eof
        then return ()
        else do
            line <- liftIO getLine
            processLine line

-- | Process a single line of input
processLine :: String -> SimpleREPL ()
processLine "" = loop
processLine ":)" = liftIO (putStrLn "You look good today!") >> loop
processLine (':':meta) = do
    case parser "meta" meta of
        Left _ -> do
            liftIO $ putStrLn $ unlines ["unrecognized meta command: :" ++ meta, hint]
            loop
        Right cmd -> (dispatchCommand cmd >> loop) `catchError` handleREPLError
processLine input = do
    case parser @Construct "interactive" input of
        Left e -> do
            liftIO . putStrLn . unlines $ [input, show e]
            loop
        Right construct -> (dispatchConstruct construct >> loop) `catchError` handleREPLError

-- | Handle REPL errors
handleREPLError :: Exception -> SimpleREPL ()
handleREPLError e = case e of
    Empty -> return ()  -- EOF, exit cleanly
    AssertFail _ -> throwError e  -- irrecoverable
    Raised _ -> throwError e  -- irrecoverable
    Info msg -> liftIO (putStrLn msg) >> loop  -- informational message
    _ -> liftIO (print e) >> loop  -- recoverable errors

-- | Dispatch IMP construct
dispatchConstruct :: Construct -> SimpleREPL ()
dispatchConstruct construct = do
    trace <- gets _trace
    state <- gets _state
    case construct of
        Statement stm -> do
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state', _trace = stm : trace}
        Arithmetic aexp -> 
            liftIO . print $ evaluate aexp state
        Boolean bexp -> 
            liftIO . putStrLn $ if evaluate bexp state then "true" else "false"
        Whitespace -> return ()

-- | Dispatch meta command
dispatchCommand :: Command -> SimpleREPL ()
dispatchCommand Quit = return ()
dispatchCommand command = case command of
    Help -> liftIO $ putStrLn $ unlines helpMessage
    Clear -> liftIO $ putStrLn $ replicate 50 '\n'  -- Simple clear
    Version -> liftIO $ putStrLn $ unwords ["impli", showVersion Paths.version]
    Reset aspect -> resetAspect aspect
    Show aspect -> showAspect aspect
    Load path -> loadFile path
    Write path -> writeTrace path
    AST element -> showAST element
    Set option -> setOption option

-- | Reset aspect of REPL state
resetAspect :: Aspect -> SimpleREPL ()
resetAspect aspect = do
    state <- gets _state
    case aspect of
        All -> do
            modify $ \st -> st {_state = initial, _trace = []}
            liftIO $ putStrLn "environment reset"
        Vars -> do
            modify $ \st -> st {_state = resetVars state}
            liftIO $ putStrLn "variables reset"
        Procs -> do
            modify $ \st -> st {_state = resetProcs state}
            liftIO $ putStrLn "procedures reset"
        Flag -> do
            modify $ \st -> st {_state = resetBreak state}
            liftIO $ putStrLn "break flag reset"
        Trace -> do
            modify $ \st -> st {_trace = []}
            liftIO $ putStrLn "trace reset"

-- | Show aspect of REPL state
showAspect :: Aspect -> SimpleREPL ()
showAspect aspect = do
    (vars, procs, flag) <- gets _state
    trace <- gets _trace
    case aspect of
        All -> showAspect Vars >> showAspect Procs >> showAspect Flag >> showAspect Trace
        Vars -> do
            liftIO $ putStrLn "Variables:"
            let varList = [k ++ " = " ++ show v | (k, v) <- Map.toList vars, case k of [] -> False; (c:_) -> c /= '_']
            liftIO $ putStrLn $ indent 4 (unlines varList)
        Procs -> do
            liftIO $ putStrLn "Procedures:"
            liftIO $ putStrLn $ indent 4 (unlines [prettify p | p <- procs])
        Flag -> liftIO $ putStrLn $ "Break: " ++ show flag ++ "\n"
        Trace -> do
            liftIO $ putStrLn "Trace:"
            let traceLines = [ init . unlines $ zipWith (++) (index : buffer) (lines s)
                             | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
                             , let index = '#' : show i ++ space 2
                                   buffer = repeat . space $ length (show i) + 3
                             ]
            liftIO $ putStrLn $ indent 4 (unlines traceLines)

-- | Load and execute IMP file
loadFile :: FilePath -> SimpleREPL ()
loadFile path = do
    content <- liftIO (readFile path) `catchError` 
        (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state <- gets _state
            trace <- gets _trace
            state' <- liftIMP $ execute (stm, state)
            modify $ \st -> st {_state = state', _trace = stm : trace}
    throwError . Info $ "interpreted: " ++ path

-- | Write trace to file
writeTrace :: FilePath -> SimpleREPL ()
writeTrace path = do
    content <- gets (prettytrace . _trace)
    liftIO (writeFile path content) `catchError` 
        (\e -> throwError . IOFail $ unlines ["write trace to: " ++ path, show e])
    throwError . Info $ "wrote trace to: " ++ path

-- | Show AST of element
showAST :: Element -> SimpleREPL ()
showAST (Input construct) = liftIO $ print construct
showAST (Index n) = do
    trace <- gets _trace
    if n <= 0 || n > length trace
        then liftIO $ putStrLn $ "index out of bounds: " ++ show n
        else liftIO $ print (trace !! (length trace - n))

-- | Set REPL option
setOption :: Option -> SimpleREPL ()
setOption option = case option of
    Welcome w -> modify $ \st -> st {_welcome = w}
    Prompt p -> modify $ \st -> st {_prompt = p}
    Separator s -> modify $ \st -> st {_separator = s}
    Goodbye g -> modify $ \st -> st {_goodbye = g}
    Verbose v -> modify $ \st -> st {_verbose = v, _separator = separator'}
        where
            separator' = case v of
                Normal -> normalsep
                Profile -> profilesep
                Debug -> debugsep

-- | Indent every line by n space characters
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | String of n space characters
space :: Int -> String
space n = replicate n ' '
