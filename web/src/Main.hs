{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Main
Description : Web capability for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Provides web capabilities for the IMP language interpreter.

---
- use WASI stdout redirection and pray it allows for streams
    - ERROR: when condition of while loop evaluates:
        - Unhandled Promise Rejection: RuntimeError: call_indirect to a null table entry (evaluating 'this.exports.execute(this.pointer,t)')
- lazily generate stdout and return each line separate (huge IO)
-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)
import Text.Read (readMaybe)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.Map as Map
import qualified GHC.Wasm.Prim as JS
import qualified Paths_impli as Paths

import Control.Monad.Except (catchError, throwError)

import Config
import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.Pretty
import IMP.Semantics.Structural
import IMP.State
import IMP.Syntax

foreign import javascript unsafe "console.log($1)" conslog :: JS.JSString -> IO ()
foreign import javascript unsafe "console.warn($1)" conswarn :: JS.JSString -> IO ()

-- | Communicate with browser console from Haskell String.
warner, logger :: String -> IMP ()
logger = liftIO . conslog . JS.toJSString
warner = liftIO . conswarn . JS.toJSString

foreign export javascript "hello" hello :: IO ()

-- | TODO
hello :: IO ()
hello = conslog $ JS.toJSString "Hello, From Haskell!"

foreign export javascript "serve" main :: IO ()

-- | Web-Entrypoint for the IMP language interpreter.
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr LineBu ffering
    repl start

-- | Environment in 'loop' as 2-tuple of trace (list of 'IMP.Syntax.Stm') and 'IMP.State.State'.
type Env = ([Stm], State)

-- | Start 'Env' for 'repl'.
start :: Env
start = ([], initial)

repl :: Env -> IO ()
repl env =
    Except.runExceptT (loop env)
        >>= either print (\_ -> putStrLn goodbye)
        >> repl env

-- | REPL loop that processes input and maintains interpreter state.
loop :: Env -> IMP ()
loop env = do
    output wwwelcome
    line <- liftIO $ getLine
    case line of
        "" -> loop env -- empty line, loop
        (':' : rest) -> handleMeta env . normalizeMeta $ words rest
        input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch env c >>= loop)
                (parser "browser" input)
        `catchError` \e -> case e of
            Empty -> output "" -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- unrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop env -- mistakes happen

-- | Process construct in environment, return updated environment.
dispatch :: Env -> Construct -> IMP Env
dispatch env@(trace, state) cnstr = case cnstr of
    Statement stm -> do
        state' <- run (stm, state)
        return (stm : trace, state')
    Arithmetic aexp -> display (evaluate state aexp) >> return env
    Boolean bexp -> output (if evaluate state bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | Help message displayed when user enters @:help@ metacommand.
helpMessage :: [String]
helpMessage =
    [ ":help / :?               Show this help message"
    , ":version                 Show the version"
    , ":clear                   Clear screen"
    , ":reset [ASPECT]          Reset environment or specific aspect (vars, procs, break, trace)"
    , ":trace                   Show trace (executed statements)"
    , ":state                   Show state (defined variables and procedures, break flag)"
    , ":load FILE               Interpret file and load resulting state"
    , ":write                   Write trace to file (opens in new tab)"
    , ":ast (INPUT | #n)        Parse and display AST of input or n-th statement in trace"
    ]

-- | Expand metacommand abbreviations.
normalizeMeta :: [String] -> [String]
normalizeMeta ["?"] = ["help"]
normalizeMeta ["h"] = ["help"]
normalizeMeta ["v"] = ["version"]
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
handleMeta :: Env -> [String] -> IMP ()
handleMeta env@(trace, state@(vars, procs, flag)) meta = case meta of
    [")"] -> output "You look good today!" >> loop env
    ["help"] -> do
        explain
            "All meta commands can be abbreviated by their first letter."
            helpMessage
        loop env
    ["version"] -> output ("impli " ++ showVersion Paths.version) >> loop env
    ["reset", it]
        | null it -> (display . Info) "environment reset" >> loop start
        | it `elem` ["v", "vars"] -> (display . Info) "variables reset" >> loop (trace, (zero, procs, flag))
        | it `elem` ["p", "procs"] -> (display . Info) "procedures reset" >> loop (trace, (vars, [], flag))
        | it `elem` ["b", "break"] -> (display . Info) "break flag reset" >> loop (trace, (vars, procs, False))
        | it `elem` ["t", "trace"] -> (display . Info) "trace reset" >> loop ([], (vars, procs, flag))
        | otherwise -> throwError . Error $ "unrecognized aspect to reset: " ++ it
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
        | null it -> throwError . Info $ "no filepath provided"
        | otherwise -> loadIMP state it >>= curry loop trace
    ["write"] -> writeIMP trace >> loop env
    ["ast", it]
        | null it -> throwError . Info $ "nothing to parse"
        | "#" <- it -> throwError . Info $ "no index provided"
        | '#' : ds <- it -> case readMaybe ds of
            Nothing -> throwError . ParseFail $ it
            Just i ->
                if i <= 0 || i > length trace
                    then throwError . Error $ "index out of bounds: " ++ show i
                    -- INFO: condition guarantees index in bounds
                    else display (trace !! (length trace - i)) >> loop env
        | otherwise -> printAST it >> loop env
    _ ->
        throwError . Error $
            unlines
                [ "not a meta command: :" ++ unwords meta
                , "Enter :help to list available metacommands and :quit to exit."
                ]

-- | Interpret IMP language source file, return updated state.
loadIMP :: State -> FilePath -> IMP State
loadIMP state path = do
    content <-
        liftIO (readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state' <- run (stm, state)
            display . Info $ "interpreted: " ++ path
            return state'

writeIMP :: [Stm] -> IMP ()
writeIMP = undefined

-- | Parse input and print AST.
printAST :: String -> IMP ()
printAST input =
    either
        (\e -> display . ParseFail $ unlines [input, show e])
        (\c -> display c)
        (parser @Construct "browser" input)

-- | Convert trace to valid IMP language source code.
prettytrace :: [Stm] -> String
prettytrace = prettify . mconcat -- Haskell is nice!

-- | Nicely format 'output' with heading and indented body.
explain :: String -> [String] -> IMP ()
explain heading [] = output heading
explain heading body = output $ heading ++ '\n' : indent 4 (unlines body)

-- | Indent every line by @n@ space characters.
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | 'String' of @n@ space characters.
space :: Int -> String
space n = replicate n ' '
