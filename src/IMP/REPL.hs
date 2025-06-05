{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{- |
Module      : IMP.REPL
Description : Read-Eval-Print Loop (REPL) for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module provides an interactive REPL for IMP, allowing users to
execute statements, evaluate expressions, display ASTs and inspect program state.
-}
module IMP.REPL where

import Control.Exception (IOException, try)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import System.Console.Haskeline hiding (display)
import System.Exit (exitFailure)

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Config
import IMP.Parser
import IMP.Pretty
import IMP.Result
import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Semantics.Statement
import IMP.Syntax

-- | Start IMP REPL with given environment.
repl :: Env -> IO ()
repl env = do
    putStrLn welcome
    runInputT settings $ do
        result <- runExceptT (loop env)
        case result of
            Left err
                | err == Ok -> return ()
                | otherwise -> liftIO $ print err >> exitFailure
            Right _ -> return ()

-- | Read input, parse, dispatch, and handle meta commands.
loop :: Env -> REPL ()
loop env = do
    line <- lift $ getInputLine prompt
    flip catchError handleThrow $
        case line of
            Nothing -> output $ goodbye -- ctrl-d
            Just "" -> loop env -- empty line, just loop
            Just (':' : meta) -> handleMeta meta env
            Just input -> case parser "interactive" input of
                Left err -> throwError $ ParseFail $ input ++ "\n" ++ show err
                Right parsed -> do
                    env' <- dispatch env parsed
                    loop env'
    where
        handleThrow err = case err of
            Ok -> throwError err
            AssFail _ -> throwError err
            Raised _ -> throwError err
            _ -> display err >> loop env

-- | Dispatch parsed construct in environment.
dispatch :: Env -> Construct -> REPL Env
dispatch env@(state, trace) construct = case construct of
    Statement s -> do
        state' <- catchError (execute state s) throwError
        return (state', trace ++ [s])
    Arithm e -> do
        display $ evaluate state e
        return env
    Bool b -> do
        output $ if evaluate state b then "true" else "false"
        return env
    Whitespace -> return env

-- | Handle meta command (starting with @:@).
handleMeta :: String -> Env -> REPL ()
handleMeta meta env = case normalizeMeta (words meta) of
    [")"] -> do
        output $ "You look good today!"
        loop env
    ["help"] -> do
        outputSection "All meta commands can be abbreviated by their first letter." helpMessage
        loop env
    ["quit"] -> output $ "Goodbye!"
    ["clear"] -> liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0) >> loop env
    ["reset", rest]
        | null rest -> output "+++ INFO: state reset" >> loop start
        | elem rest ["v", "vars"] -> output "+++ INFO: variables reset " >> loop novars
        | elem rest ["p", "procs"] -> output "+++ INFO: procedures reset " >> loop noprocs
        | elem rest ["b", "break"] -> output "+++ INFO: break flag reset " >> loop nobreak
        | elem rest ["t", "trace"] -> output "+++ INFO: trace reset " >> loop notrace
        | otherwise -> throwError $ Error $ "unrecognized subcommand: " ++ rest
        where
            (vs, ps, fl) = st env
            trace = tr env
            novars = ((Map.empty, ps, fl), trace)
            noprocs = ((vs, [], fl), trace)
            nobreak = ((vs, ps, False), trace)
            notrace = ((vs, ps, fl), [])
    ["trace"] -> do
        outputSection
            "Trace:"
            [ (init . unlines) $ zipWith (++) (idx : buf) ls
            | (i, s) <- zip [1 :: Int ..] (tr env)
            , let
                len = length (show i) + 3
                idx = "#" ++ show i ++ space 2
                buf = repeat $ space len
                ls = lines $ prettify s
            ]
        loop env
    ["state"] -> do
        let (vars, procs, flag) = st env
        outputSection "Variables:" [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        outputSection "Procedures:" [prettify p | p <- procs]
        output $ "Break: " ++ show flag
        loop env
    ["load", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> do
            state <- readIMP (st env) path -- handle interrupt
            loop (state, tr env)
    ["write", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> writeIMP (tr env) path >> loop env
    ["ast", input]
        | null input -> throwError $ Info "nothing to parse"
        | "#" <- input -> throwError $ Info "no index provided"
        | '#' : ds <- input -> case parseIndex ds of
            Nothing -> throwError $ ParseFail input
            Just i ->
                if i <= 0 || i > length (tr env)
                    then throwError $ Error $ "index out of bounds: " ++ show i
                    else output (show $ tr env !! (i - 1)) >> loop env
        | otherwise -> printAST input >> loop env
    _ ->
        throwError $
            Error $
                unlines
                    [ "not a meta command: :" ++ meta
                    , indent 4 "to list available options enter :help"
                    ]

-- | Normalize meta command from alias to full form.
normalizeMeta :: [String] -> [String]
normalizeMeta ws = case ws of
    ["?"] -> ["help"]
    ["h"] -> ["help"]
    ["q"] -> ["quit"]
    ["c"] -> ["clear"]
    ["r"] -> ["reset"]
    ["t"] -> ["trace"]
    ["s"] -> ["state"]
    (x : xs)
        | elem x ["l", "load"] -> ["load", rest]
        | elem x ["w", "write"] -> ["write", rest]
        | elem x ["a", "ast"] -> ["ast", rest]
        | elem x ["r", "reset"] -> ["reset", rest]
        where
            rest = unwords xs
    _ -> ws

-- | Help message for meta commands.
helpMessage :: [String]
helpMessage =
    [ ""
    , ":help / :?           Show this help"
    , ":quit                Quit REPL"
    , ":clear               Clear screen"
    , ":reset [ELEMENT]     Reset environment or element (vars, procs, break, trace)"
    , ":trace               Show trace (executed statements)"
    , ":state               Show state (defined variables and procedures)"
    , ":load FILE           Interpret file and load resulting state"
    , ":write FILE          Write trace to file (relative to CWD)"
    , ":ast (INPUT | #n)    Parse and display AST of input or n-th statement in trace"
    ]

-- | Parse and display AST of given input string.
printAST :: String -> REPL ()
printAST input = case parser "ast" input of
    Left err -> throwError $ ParseFail $ input ++ "\n" ++ show err
    Right (parsed :: Construct) -> display parsed

-- | Prettyprint list of statements as a trace.
prettyTrace :: [Stm] -> String
prettyTrace [] = "skip\n"
prettyTrace stms = unlines $ map (++ ";") (init strs) ++ [last strs]
    where
        strs = map prettify stms

-- | Interpret IMP source file, updating state.
readIMP :: State -> FilePath -> REPL State
readIMP state path = do
    result <- liftIO $ try (readFile path) :: REPL (Either IOException String)
    case result of
        Left err -> throwError $ IOFail $ "read from: " ++ path ++ "\n" ++ show err
        Right content -> case parser path content of
            Left err -> throwError $ IOFail $ "load file: " ++ path ++ "\n" ++ show err
            Right stm -> do
                state' <- execute state stm
                output $ "+++ INFO: interpreted file: " ++ path
                return state'

-- | Write trace to a file as valid IMP program.
writeIMP :: [Stm] -> FilePath -> REPL ()
writeIMP stms path = liftIO $ do
    let content = prettyTrace stms
    result <- try (writeFile path content) :: IO (Either IOException ())
    case result of
        Left err -> do
            print $ IOFail $ "write trace to: " ++ path
            print err
        Right () -> do
            print $ Info $ "wrote trace to: " ++ path

-- | Parse string as positive integer index.
parseIndex :: String -> Maybe Int
parseIndex ds
    | all (`elem` ['0' .. '9']) ds = Just (read ds)
    | otherwise = Nothing

-- | Produce string of n spaces.
space :: Int -> String
space n = replicate n ' '

-- | Indent each line of a string by n spaces.
indent :: Int -> String -> String
indent n = (init . unlines) . map (space n ++) . lines

-- | Output a titled section with optional indented content.
outputSection :: String -> [String] -> REPL ()
outputSection title section =
    output $
        title
            ++ if (not . null) section
                then "\n" ++ indent 4 (unlines section)
                else ""
