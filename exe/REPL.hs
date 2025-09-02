{-# LANGUAGE TupleSections #-}
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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import qualified Control.Monad.Except as Except
import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as Haskeline

import Config
import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.State
import IMP.Statement
import IMP.Syntax

-- | Encapsulation of computation in 'repl'.
type REPL = Except.ExceptT Exception (Haskeline.InputT IO)

-- | Environment in 'repl' as 2-tuple of trace (list of 'IMP.Syntax.Stm') and 'IMP.State.State'.
type Env = ([Stm], State)

-- | Start 'Env' for 'repl'.
start :: Env
start = ([], initial)

-- | Lift computation from 'IMP.State.IMP' into 'REPL'.
liftIMP :: IMP a -> REPL a
liftIMP = Except.ExceptT . lift . Except.runExceptT

-- | Default settings for 'repl'.
-- CHECK: custom `complete` function here.
settings :: Bool -> Haskeline.Settings IO
settings flag =
    Haskeline.defaultSettings {Haskeline.historyFile = if flag then Nothing else historyFile}

-- | REPL entrypoint with custom settings and environment.
repl :: Haskeline.Settings IO -> Env -> IO ()
repl s env = do
    putStrLn welcome
    Haskeline.runInputT s (Haskeline.withInterrupt $ Except.runExceptT (loop env))
        >>= either
            (\e -> print e >> exitFailure)
            (\_ -> putStrLn goodbye)

-- | REPL loop that processes input and maintains interpreter state.
loop :: Env -> REPL ()
loop env = Haskeline.handleInterrupt (loop env) $ do
    line <- lift . Haskeline.getInputLine $ prompt
    case line of
        Nothing -> return () -- ctrl-d, exit cleanly
        Just "" -> loop env -- empty line, loop
        Just (':' : rest) -> handleMeta env . normalizeMeta $ words rest
        Just input ->
            either
                (\e -> Except.throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch env c >>= loop)
                (parser "interactive" input)
        `Except.catchError` \e -> case e of
            Empty -> flush -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> Except.throwError e -- unrecoverable, propagate
            Raised _ -> Except.throwError e -- ''
            _ -> display e >> loop env -- mistakes happen

-- | Process construct in environment, return updated environment.
dispatch :: Env -> Construct -> REPL Env
dispatch env@(trace, state) cnstr = case cnstr of
    Statement stm ->
        Haskeline.handleInterrupt
            (flush >> return env) -- INFO: statement isn't added to trace after interrupt
            ((stm : trace,) <$> liftIMP (execute (stm, state)))
    Arithmetic aexp -> display (evaluate state aexp) >> return env
    Boolean bexp -> output (if evaluate state bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | Help message displayed on @:help@ metacommand.
helpMessage :: [String]
helpMessage =
    [ ":help / :?               Show this help message"
    , ":quit                    Quit REPL"
    , ":clear                   Clear screen"
    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
    , ":trace                   Show trace (executed statements)"
    , ":state                   Show state (defined variables and procedures, break flag)"
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
    | w `elem` ["r", "reset"] = ["reset", it]
    where
        it = unwords ws
normalizeMeta rest = rest

-- | Process metacommand in environment, continue loop or exit.
handleMeta :: Env -> [String] -> REPL ()
handleMeta env@(trace, state@(vars, procs, flag)) meta = case meta of
    [")"] -> output "You look good today!" >> loop env
    ["help"] -> do
        explain
            "All meta commands can be abbreviated by their first letter."
            helpMessage
        loop env
    ["quit"] -> return ()
    ["clear"] -> clear >> loop env
    ["reset", it]
        | null it -> (display . Info) "environment reset" >> loop start
        | it `elem` ["v", "vars"] -> (display . Info) "variables reset" >> loop (trace, (zero, procs, flag))
        | it `elem` ["p", "procs"] -> (display . Info) "procedures reset" >> loop (trace, (vars, [], flag))
        | it `elem` ["b", "break"] -> (display . Info) "break flag reset" >> loop (trace, (vars, procs, False))
        | it `elem` ["t", "trace"] -> (display . Info) "trace reset" >> loop ([], (vars, procs, flag))
        | otherwise -> liftIMP . errata $ "unrecognized aspect to reset: " ++ it
    ["trace"] -> do
        -- CHECK: is there some better way to do this without reverse?
        explain
            "Trace:"
            [ init . unlines $ zipWith (++) (indx : bufs) (lines s)
            | (i, s) <- zip [1 :: Int ..] (reverse . map prettify $ trace)
            , let
                indx = '#' : show i ++ space 2
                bufs = repeat . space $ length (show i) + 3
            ]
        loop env
    ["state"] -> do
        explain
            "Variables:"
            -- INFO: invariant of IMP.State.setVar guarantees no empty string key
            [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        explain "Procedures:" [prettify p | p <- procs]
        output $ "Break: " ++ show flag ++ "\n"
        loop env
    ["load", it]
        | null it -> Except.throwError . Info $ "no filepath provided"
        | otherwise -> loadIMP state it >>= curry loop trace
    ["write", it]
        | null it -> Except.throwError . Info $ "no filepath provided"
        | otherwise -> writeIMP trace it >> loop env
    ["ast", it]
        | null it -> Except.throwError . Info $ "nothing to parse"
        | "#" <- it -> Except.throwError . Info $ "no index provided"
        | '#' : ds <- it -> case readMaybe ds of
            Nothing -> Except.throwError . ParseFail $ it
            Just i ->
                if i <= 0 || i > length trace
                    then Except.throwError $ Error $ "index out of bounds: " ++ show i
                    -- INFO: condition guarantees index in bounds
                    else display (trace !! (length trace - i)) >> loop env
        | otherwise -> liftIO (printAST it) >> loop env
    _ ->
        liftIMP . errata $
            unlines
                [ "not a meta command: :" ++ unwords meta
                , "Enter :help to list available metacommands and :quit to exit."
                ]

-- | Interpret IMP language source file, return updated state.
loadIMP :: State -> FilePath -> REPL State
loadIMP state path = do
    content <-
        liftIO (readFile path)
            `Except.catchError` (\e -> Except.throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> Except.throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state' <- liftIMP $ execute (stm, state)
            display . Info $ "interpreted: " ++ path
            return state'

-- | Write trace to specified file.
writeIMP :: [Stm] -> FilePath -> REPL ()
writeIMP trace path = do
    let content = prettytrace trace
    _ <-
        liftIO (writeFile path content)
            `Except.catchError` (\e -> Except.throwError . IOFail $ unlines ["write trace to: " ++ path, show e])
    Except.throwError . Info $ "wrote trace to: " ++ path

-- | Parse input and print AST.
printAST :: String -> IO ()
printAST input =
    either
        (\e -> print . ParseFail $ unlines [input, show e])
        print
        (parser @Construct "interactive" input)

-- | Convert trace to valid IMP language source code.
prettytrace :: [Stm] -> String
prettytrace = prettify . mconcat -- Haskell is nice!

-- | Clear the REPL.
clear :: REPL ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)

-- | Flush the REPL.
flush :: REPL ()
flush = output ""

-- | 'putStrLn' inside 'REPL'.
output :: String -> REPL ()
output = liftIO . putStrLn

-- | 'print' inside 'REPL'.
display :: (Show a) => a -> REPL ()
display = liftIO . print

-- | Nicely format 'output' with heading and indented body.
explain :: String -> [String] -> REPL ()
explain heading [] = output heading
explain heading body = output $ heading ++ '\n' : indent 4 (unlines body)

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
