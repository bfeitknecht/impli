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
interpret statements, evaluate expressions, display ASTs and inspect program state.
It uses configuration settings from "IMP.Config", parsing functionality from "IMP.Parser",
and semantic interpretation from "IMP.Semantic.Statement" to execute IMP programs interactively.
-}
module IMP.REPL (
    repl,
    loop,
    dispatch,
    handleMeta,
    printAST,
) where

import Control.Exception (IOException, try)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import qualified System.Console.Haskeline as Haskeline
import System.Exit (exitFailure)

import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

import IMP.Config
import IMP.Parser
import IMP.Pretty
import IMP.Result
import IMP.Semantic.Expression
import IMP.Semantic.State
import IMP.Semantic.Statement
import IMP.Syntax
import IMP.Util

-- | Start IMP REPL with given environment.
-- Takes 'Haskeline.Settings' for "System.Console.Haskeline" from "IMP.Config" and initial 'Env'.
repl :: Haskeline.Settings IO -> Env -> IO ()
repl setting env = do
    putStrLn welcome
    Haskeline.runInputT setting $ runExceptT (loop env) >> return ()

-- | Read, parse, dispatch input and handle meta commands.
-- This continuously processes user input until termination is requested or unrecoverable error occurs.
loop :: Env -> REPL ()
loop env = do
    line <- lift $ Haskeline.getInputLine prompt
    flip catchError handleThrow $
        case line of
            Nothing -> output goodbye -- ctrl-d
            Just "" -> loop env -- empty line, just loop
            Just (':' : meta) -> handleMeta meta env
            Just input -> case parser "interactive" input of
                Left err -> throwError $ ParseFail $ unlines' [input, show err]
                Right parsed -> do
                    env' <- dispatch env parsed
                    loop env'
    where
        handleThrow err = case err of
            Ok -> return () -- ctrl+d encountered
            AssFail _ -> display err >> liftIO exitFailure
            Raised _ -> display err >> liftIO exitFailure
            _ -> display err >> loop env

-- | Dispatch parsed 'Construct' in given environment, 'Env'. Interprets statements with "IMP.Semantic.State"
-- and evaluates expressions using "IMP.Semantic.Expression", then displays result to the user.
dispatch :: Env -> Construct -> REPL Env
dispatch env@(state, trace) construct = case construct of
    Statement s -> do
        state' <- catchError (interpret state s) throwError
        return (state', trace ++ [s])
    Arithmetic e -> display (evaluate state e) >> return env
    Boolean b -> output (if evaluate state b then "true" else "false") >> return env
    Whitespace -> return env

-- | Handle meta command (starting with @:@). Processes special commands that control the REPL.
handleMeta :: String -> Env -> REPL ()
handleMeta meta env@(state, trace) = case normalizeMeta (words meta) of
    [")"] -> do
        output "You look good today!"
        loop env
    ["help"] -> do
        outputSection "All meta commands can be abbreviated by their first letter." helpMessage
        loop env
    ["quit"] -> output goodbye
    ["clear"] -> liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0) >> loop env
    ["reset", rest]
        | null rest -> output "+++ INFO: environment reset" >> loop start
        | elem rest ["v", "vars"] -> output "+++ INFO: variables reset " >> loop (novars state, trace)
        | elem rest ["p", "procs"] -> output "+++ INFO: procedures reset " >> loop (noprocs state, trace)
        | elem rest ["b", "break"] -> output "+++ INFO: break flag reset " >> loop (nobreak state, trace)
        | elem rest ["t", "trace"] -> output "+++ INFO: trace reset " >> loop (state, [])
        | otherwise -> throwError $ Error $ "unrecognized aspect to reset: " ++ rest
    ["trace"] -> do
        outputSection
            "Trace:"
            [ unlines' $ zipWith (++) (idx : buf) ls
            | (i, s) <- zip [1 :: Int ..] trace
            , let
                len = length (show i) + 3
                idx = "#" ++ show i ++ space 2
                buf = repeat $ space len
                ls = lines $ prettify s
            ]
        loop env
    ["state"] -> do
        let (vars, procs, flag) = state
        outputSection
            "Variables:"
            -- IMP.State.setVar invariant guarantees no empty string key
            [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        outputSection "Procedures:" [prettify p | p <- procs]
        output $ "Break: " ++ show flag
        loop env
    ["load", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> do
            state' <- readIMP state path -- handle interrupt
            loop (state', trace)
    ["write", path]
        | null path -> throwError $ Info "no filepath provided"
        | otherwise -> writeIMP trace path >> loop env
    ["ast", input]
        | null input -> throwError $ Info "nothing to parse"
        | "#" <- input -> throwError $ Info "no index provided"
        | '#' : ds <- input -> case parseIndex ds of
            Nothing -> throwError $ ParseFail input
            Just i ->
                if i <= 0 || i > length trace
                    then throwError $ Error $ "index out of bounds: " ++ show i
                    else display (trace !! (i - 1)) >> loop env
        | otherwise -> printAST input >> loop env
    _ ->
        throwError $
            Error $
                unlines'
                    [ "not a meta command: :" ++ meta
                    , "Enter :help to list available metacommands and :quit to exit."
                    ]

-- | Normalize meta command from alias to full form.
-- Allows users to type abbreviated versions of commands like @:h@ instead of @:help@.
normalizeMeta :: [String] -> [String]
normalizeMeta metas = case metas of
    ["?"] -> ["help"]
    ["h"] -> ["help"]
    ["q"] -> ["quit"]
    ["c"] -> ["clear"]
    ["t"] -> ["trace"]
    ["s"] -> ["state"]
    (w : ws)
        | elem w ["l", "load"] -> ["load", rest]
        | elem w ["w", "write"] -> ["write", rest]
        | elem w ["a", "ast"] -> ["ast", rest]
        | elem w ["r", "reset"] -> ["reset", rest]
        where
            rest = unwords ws
    _ -> metas

-- | Help message for meta commands. Displayed when user enters @:help@ or @:?@ command.
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

-- | Parse and display AST of given input string. Uses "IMP.Parser" to parse the input
-- and displays the resulting abstract syntax tree to help users understand program structure.
printAST :: String -> REPL ()
printAST input = case parser "ast" input of
    Left err -> throwError $ ParseFail $ unlines' [input, show err]
    Right (parsed :: Construct) -> display parsed

-- | Convert Trace to IMP source code. Uses "IMP.Pretty" to format the execution trace
-- as valid IMP source code that can be saved to file.
prettyTrace :: Trace -> String
prettyTrace [] = "skip\n"
prettyTrace trace =
    unlines $
        map ((++ ";") . prettify) (init trace) ++ [prettify $ last trace]

-- | Interpret IMP source file, updating state. Reads IMP code from file,
-- parses it using "IMP.Parser" and interprets it using "IMP.Semantic.Statement".
readIMP :: State -> FilePath -> REPL State
readIMP state path = do
    result <- liftIO $ try (readFile path) :: REPL (Either IOException String)
    case result of
        Left err -> throwError $ IOFail $ unlines' ["read file: " ++ path, show err]
        Right content -> case parser path content of
            Left err -> throwError $ IOFail $ unlines' ["interpret file: " ++ path, show err]
            Right stm -> do
                state' <- interpret state stm
                output $ "+++ INFO: interpreted file: " ++ path
                return state'

-- | Write trace to a file as valid IMP program. Saves the execution history
-- as an IMP program that can be loaded and interpreted again.
writeIMP :: Trace -> FilePath -> REPL ()
writeIMP trace path = liftIO $ do
    let content = prettyTrace trace
    result <- try (writeFile path content) :: IO (Either IOException ())
    case result of
        Left err -> print $ IOFail $ unlines' ["write trace to: " ++ path, show err]
        Right () -> print $ Info $ unlines' ["wrote trace to: " ++ path]

-- | Parse string as positive integer index.
-- Used by the @:ast@ command to interpret
-- numeric indices referring to statements in the trace.
parseIndex :: String -> Maybe Int
parseIndex ds
    | all (`elem` ['0' .. '9']) ds = Just (read ds)
    | otherwise = Nothing

-- | Produce string of @n@ spaces.
-- Used for formatting indented output in the REPL.
space :: Int -> String
space n = replicate n ' '

-- | Indent each line of a string by @n@ spaces.
-- Used to format multi-line output.
indent :: Int -> String -> String
indent n = unlines' . map (space n ++) . lines

-- | Output a titled section with optional indented content.
-- Used for displaying
-- structured information like defined state or the help text.
outputSection :: String -> [String] -> REPL ()
outputSection title section =
    output $
        title
            ++ if section /= []
                then "\n" ++ indent 4 (unlines section)
                else ""
