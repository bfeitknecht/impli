{-# LANGUAGE ScopedTypeVariables #-}

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

import IMP.Parser
import IMP.Pretty
import IMP.Result
import IMP.Semantics.Expression
import IMP.Semantics.State
import IMP.Semantics.Statement
import IMP.Syntax

repl :: Env -> IO ()
repl env =
    runInputT defaultSettings $ do
        result <- runExceptT (loop env)
        case result of
            Left Ok -> return ()
            Left err -> liftIO $ print err >> exitFailure
            Right _ -> return ()

loop :: Env -> REPL ()
loop env = do
    line <- lift $ getInputLine "IMP> "
    flip catchError handleThrow $
        case line of
            Nothing -> output $ "Goodbye!" -- ctrl-d
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

handleMeta :: String -> Env -> REPL ()
handleMeta meta env = case normalizeMeta (words meta) of
    [")"] -> do
        output $ "You look good today!"
        loop env
    ["help"] -> do
        outputSection "All meta commands can be abbreviated by their first letter." helpMessage
        loop env
    ["quit"] -> output $ "Goodbye!"
    ["clear"] -> do
        liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)
        loop env
    ["reset", rest]
        | null rest -> output "+++ INFO: state reset" >> loop start
        | elem rest ["v", "vars"] -> output "+++ INFO: variables reset " >> loop novars
        | elem rest ["p", "procs"] -> output "+++ INFO: procedures reset " >> loop noprocs
        | elem rest ["b", "break"] -> output "+++ INFO: break flag reset " >> loop nobreak
        | elem rest ["t", "trace"] -> output "+++ INFO: trace reset " >> loop notrace
        | otherwise -> throwError $ Error $ "*** ERROR: unrecognized subcommand: " ++ rest
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
            , let len = length (show i) + 3
            , let idx = "#" ++ show i ++ space 2
            , let buf = repeat $ space len
            , let ls = lines $ prettify s
            ]
        loop env
    ["state"] -> do
        let (vars, procs, flag) = st env
        outputSection "Variables:" [k ++ " = " ++ show v | (k, v) <- Map.toList vars]
        outputSection "Procedures:" [prettify p | p <- procs]
        output $ "Break: " ++ show flag
        loop env
    ["load", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> do
            result <- loadIMP (st env) path
            case result of
                Nothing -> throwError $ IOFail $ "load file: " ++ path
                Just state -> loop (state, tr env)
    ["write", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> (writeIMP (tr env) path) >> loop env
    ["ast", input]
        | null input -> throwError $ Info "nothing to parse"
        | "#" <- input -> throwError $ Info "no index provided"
        | '#' : ds <- input -> case parseIndex ds of
            Nothing -> throwError $ ParseFail input
            Just i ->
                if i <= 0 || i > length (tr env)
                    then throwError $ Error $ "index out of bounds: " ++ show i
                    else do
                        output (show $ tr env !! (i - 1))
                        loop env
        | otherwise -> (printAST input) >> loop env
    _ ->
        throwError $
            Error $
                unlines
                    [ "not a meta command: :" ++ meta
                    , indent 4 "to list available options enter :help"
                    ]

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
        | x == "l" || x == "load" -> ["load", rest]
        | x == "w" || x == "write" -> ["write", rest]
        | x == "a" || x == "ast" -> ["ast", rest]
        | x == "r" || x == "reset" -> ["reset", rest]
        where
            rest = unwords xs
    _ -> ws

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

printAST :: String -> REPL ()
printAST input = case parser "ast" input of
    Left err -> throwError $ ParseFail $ input ++ "\n" ++ show err
    Right (parsed :: Construct) -> display parsed

prettyTrace :: [Stm] -> String
prettyTrace [] = "skip\n"
prettyTrace stms = unlines $ map (++ ";") (init strs) ++ [last strs]
    where
        strs = map prettify stms

loadIMP :: State -> FilePath -> REPL (Maybe State)
loadIMP state path = do
    result <- liftIO $ try (readFile path) :: REPL (Either IOException String)
    case result of
        Left err -> do
            output $ "*** ERROR: IO failure while reading from: " ++ path
            display err
            return Nothing
        Right content -> case parser path content of
            Left err -> do
                output $ "*** ERROR: parse failure in: " ++ path
                display err
                return Nothing
            Right stm -> do
                state' <- execute state stm
                output $ "+++ INFO: interpreted file: " ++ path
                return $ Just state'

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

parseIndex :: String -> Maybe Int
parseIndex ds
    | all (`elem` ['0' .. '9']) ds = Just (read ds)
    | otherwise = Nothing

space :: Int -> String
space n = replicate n ' '

indent :: Int -> String -> String
indent n = (init . unlines) . map (space n ++) . lines

outputSection :: String -> [String] -> REPL ()
outputSection title section =
    output $
        title
            ++ if (not . null) section
                then "\n" ++ indent 4 (unlines section)
                else ""
