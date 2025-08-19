{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module IMP2.REPL where

import qualified Control.Monad.Trans.Except as Except
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as Haskeline

import qualified Data.Map as Map
import System.Exit
import Text.Read (readMaybe)

import Control.Monad.Except (catchError, throwError)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import Config
import IMP2.Exception
import IMP2.Expression
import IMP2.Parser
import IMP2.Pretty
import IMP2.State
import IMP2.Statement
import IMP2.Syntax

type Env = ([Stm], State)

type REPL = Except.ExceptT Exception (Haskeline.InputT IO)

-- | TODO
start :: Env
start = ([], initial)

-- | Lift IMP computation into 'REPL' monad.
liftIMP :: IMP a -> REPL a
liftIMP = Except.ExceptT . lift . Except.runExceptT

-- | TODO
repl :: Haskeline.Settings IO -> Env -> IO ()
repl cfg env = do
    putStrLn welcome
    Haskeline.runInputT cfg (Except.runExceptT (loop env))
        >>= either
            (\e -> print e >> exitFailure)
            return

-- | TODO
loop :: Env -> REPL ()
loop env = do
    line <- lift $ Haskeline.getInputLine prompt
    case line of
        Nothing -> output goodbye -- ctrl-d, exit cleanly
        Just "" -> loop env -- empty line, loop
        Just (':' : rest) -> handleMeta env . normalizeMeta $ words rest
        Just input ->
            either
                (\e -> throwError . ParseFail $ unlines [input, show e])
                (\c -> dispatch env c >>= loop)
                (parser "interactive" input)
                `catchError` \e -> case e of
                    Empty -> output ('\n' : goodbye) >> return () -- ctrl-d during read, exit cleanly
                    AssertFail _ -> throwError e -- unrecoverable, propagate
                    Raised _ -> throwError e -- ''
                    _ -> display e >> loop env -- mistakes happen

-- | TODO
dispatch :: Env -> Construct -> REPL Env
dispatch env@(trace, state) cnstr = case cnstr of
    Statement stm -> do
        state' <- liftIMP $ interpret (stm, state)
        return (stm : trace, state')
    Arithmetic aexp -> display (evaluate state aexp) >> return env
    Boolean bexp -> output (if evaluate state bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | TODO
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

-- | TODO
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

-- | TODO
-- CHECK: rewrite with case expression anyway?
handleMeta :: Env -> [String] -> REPL ()
handleMeta env@(trace, state@(vars, procs, flag)) meta = case meta of
    [")"] -> output "You look good today!" >> loop env
    ["help"] -> do
        outputSection
            "All meta commands can be abbreviated by their first letter."
            helpMessage
        loop env
    ["quit"] -> output goodbye
    ["clear"] -> clear >> loop env
    ["reset", it]
        | null it -> (display . Info) "environment reset" >> loop start
        | it `elem` ["v", "vars"] -> (display . Info) "variables reset" >> loop (trace, (zero, procs, flag))
        | it `elem` ["p", "procs"] -> (display . Info) "procedures reset" >> loop (trace, (vars, [], flag))
        | it `elem` ["b", "break"] -> (display . Info) "break flag reset" >> loop (trace, (vars, procs, False))
        | it `elem` ["t", "trace"] -> (display . Info) "trace reset" >> loop ([], (vars, procs, flag))
        | otherwise -> throwError . Error $ "unrecognized aspect to reset: " ++ it
    ["trace"] -> do
        outputSection
            "Trace:"
            [ unlines $ zipWith (++) (idx : buf) ls
            | (i, s) <- zip [1 :: Int ..] trace
            , let
                len = length (show i) + 3
                idx = "#" ++ show i ++ space 2
                buf = repeat $ space len
                ls = lines $ prettify s
            ]
        loop env
    ["state"] -> do
        outputSection
            "Variables:"
            -- invariant of IMP2.State.setVar guarantees no nempty string key
            [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
        outputSection "Procedures:" [prettify p | p <- procs]
        output $ "Break: " ++ show flag
        loop env
    ["load", it]
        | null it -> throwError . Info $ "no filepath provided"
        | otherwise -> loadIMP state it >>= curry loop trace
    ["write", it]
        | null it -> throwError . Info $ "no filepath provided"
        | otherwise -> writeIMP trace it >> loop env
    ["ast", it]
        | null it -> throwError . Info $ "nothing to parse"
        | "#" <- it -> throwError . Info $ "no parseIndex provided"
        | '#' : ds <- it -> case readMaybe ds of
            Nothing -> throwError . ParseFail $ it
            Just i ->
                if i <= 0 || i > length trace
                    then throwError $ Error $ "parseIndex out of bounds: " ++ show i
                    else display (trace !! (i - 1)) >> loop env
        | otherwise -> liftIO (printAST it) >> loop env
    _ ->
        throwError . Error $
            unlines
                [ "not a meta command: :" ++ unwords meta
                , "Enter :help to list available metacommands and :quit to exit."
                ]

-- | TODO
loadIMP :: State -> FilePath -> REPL State
loadIMP state path = do
    content <-
        liftIO (readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state' <- liftIMP $ interpret (stm, state)
            display . Info $ "interpreted: " ++ path
            return state'

-- | TODO
writeIMP :: [Stm] -> FilePath -> REPL ()
writeIMP trace path = do
    let content = prettytrace trace
    _ <-
        liftIO (writeFile path content)
            `catchError` (\e -> throwError . IOFail $ unlines ["write to: " ++ path, show e])
    throwError . Info $ "wrote to: " ++ path

-- | TODO
printAST :: String -> IO ()
printAST input = case parser "ast" input of
    Left e -> print . ParseFail $ unlines [input, show e]
    Right (c :: Construct) -> print c

-- | TODO
prettytrace :: [Stm] -> String
prettytrace trace = prettify $ foldr Seq Skip trace

-- | TODO
clear :: REPL ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)

-- | TODO
output :: String -> REPL ()
output = liftIO . putStrLn

-- | TODO
display :: (Show a) => a -> REPL ()
display = output . show

-- CHECK newline shenanigans
outputSection :: String -> [String] -> REPL ()
outputSection title [] = output title
outputSection title par = output $ title ++ '\n' : indent 4 (unlines par)

-- | TODO
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | TODO
space :: Int -> String
space n = replicate n ' '
