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

type Env = (State, [Stm])

type REPL = Except.ExceptT Exception (Haskeline.InputT IO)

-- | __TODO__
start :: Env
start = (initial, [])

-- | Lift IMP computation into 'REPL' monad.
liftIMP :: IMP a -> REPL a
liftIMP = Except.ExceptT . lift . Except.runExceptT

-- | __TODO__
repl :: Haskeline.Settings IO -> Env -> IO ()
repl cfg env = do
    putStrLn welcome
    Haskeline.runInputT cfg (Except.runExceptT (loop env))
        >>= either
            (\e -> print e >> exitFailure)
            return

-- | __TODO__
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

-- | __TODO__
dispatch :: Env -> Construct -> REPL Env
dispatch env@(state, trace) cnstr = case cnstr of
    Statement stm -> do
        state' <- liftIMP $ interpret state stm
        return (state', stm : trace)
    Arithmetic aexp -> display (evaluate state aexp) >> return env
    Boolean bexp -> output (if evaluate state bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | __TODO__
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

-- | __TODO__
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

-- | __TODO__
handleMeta :: Env -> [String] -> REPL ()
handleMeta env [")"] = output "You look good today!" >> loop env
handleMeta env ["help"] = do
    outputSection
        "All meta commands can be abbreviated by their first letter."
        helpMessage
    loop env
handleMeta _ ["quit"] = output goodbye
handleMeta env ["clear"] = clear >> loop env
handleMeta ((vars, procs, flag), trace) ["reset", it]
    | null it = (display . Info) "environment reset" >> loop start
    | it `elem` ["v", "vars"] = (display . Info) "variables reset" >> loop ((zero, procs, flag), trace)
    | it `elem` ["p", "procs"] = (display . Info) "procedures reset" >> loop ((vars, [], flag), trace)
    | it `elem` ["b", "break"] = (display . Info) "break flag reset" >> loop ((vars, procs, False), trace)
    | it `elem` ["t", "trace"] = (display . Info) "trace reset" >> loop ((vars, procs, flag), [])
    | otherwise = throwError . Error $ "unrecognized aspect to reset: " ++ it
handleMeta env@(_, trace) ["trace"] = do
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
handleMeta env@((vars, procs, flag), _) ["state"] = do
    outputSection
        "Variables:"
        -- invariant of IMP2.State.setVar guarantees no nempty string key
        [k ++ " = " ++ show v | (k, v) <- Map.toList vars, head k /= '_']
    outputSection "Procedures:" [prettify p | p <- procs]
    output $ "Break: " ++ show flag
    loop env
handleMeta (state, trace) ["load", it]
    | null it = throwError . Info $ "no filepath provided"
    | otherwise = do
        state' <- loadIMP state it
        loop (state', trace)
handleMeta env@(_, trace) ["write", it]
    | null it = throwError . Info $ "no filepath provided"
    | otherwise = writeIMP trace it >> loop env
handleMeta env@(_, trace) ["ast", it]
    | null it = throwError . Info $ "nothing to parse"
    | "#" <- it = throwError . Info $ "no parseIndex provided"
    | '#' : ds <- it = case readMaybe ds of
        Nothing -> throwError . ParseFail $ it
        Just i ->
            if i <= 0 || i > length trace
                then throwError $ Error $ "parseIndex out of bounds: " ++ show i
                else display (trace !! (i - 1)) >> loop env
    | otherwise = liftIO (printAST it) >> loop env
handleMeta _ meta =
    throwError . Error $
        unlines
            [ "not a meta command: :" ++ unwords meta
            , "Enter :help to list available metacommands and :quit to exit."
            ]

-- | __TODO__
loadIMP :: State -> FilePath -> REPL State
loadIMP state path = do
    content <-
        liftIO (readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            state' <- liftIMP $ interpret state stm
            display . Info $ "interpreted: " ++ path
            return state'

-- | __TODO__
writeIMP :: [Stm] -> FilePath -> REPL ()
writeIMP trace path = do
    let content = prettytrace trace
    _ <-
        liftIO (writeFile path content)
            `catchError` (\e -> throwError . IOFail $ unlines ["write to: " ++ path, show e])
    throwError . Info $ "wrote to: " ++ path

-- | __TODO__
printAST :: String -> IO ()
printAST input = case parser "ast" input of
    Left e -> print . ParseFail $ unlines [input, show e]
    Right (c :: Construct) -> print c

-- | __TODO__
prettytrace :: [Stm] -> String
prettytrace trace = prettify $ foldr Seq Skip trace

-- | __TODO__
clear :: REPL ()
clear = liftIO (ANSI.clearScreen >> ANSI.setCursorPosition 0 0)

-- | __TODO__
output :: String -> REPL ()
output = liftIO . putStrLn

-- | __TODO__
display :: (Show a) => a -> REPL ()
display = output . show

-- __CHECK__ newline shenanigans
outputSection :: String -> [String] -> REPL ()
outputSection title [] = output title
outputSection title par = output $ title ++ '\n' : indent 4 (unlines par)

-- | __TODO__
indent :: Int -> String -> String
indent n = unlines . map (space n ++) . lines

-- | __TODO__
space :: Int -> String
space n = replicate n ' '
