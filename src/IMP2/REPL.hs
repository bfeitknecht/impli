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

-- | Lift IMP computation into REPL monad.
liftIMP :: IMP a -> REPL a
liftIMP imp = (liftIO . Except.runExceptT) imp >>= either throwError return

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
dispatch env@(st, tr) cnstr = case cnstr of
    Statement stm -> do
        st' <- liftIMP $ interpret st stm
        return (st', stm : tr)
    Arithmetic aexp -> display (evaluate st aexp) >> return env
    Boolean bexp -> output (if evaluate st bexp then "true" else "false") >> return env
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
handleMeta env ["help"] =
    outputSection
        "All meta commands can be abbreviated by their first letter."
        helpMessage
        >> loop env
handleMeta _ ["quit"] = output goodbye
handleMeta env ["clear"] = clear >> loop env
handleMeta ((vs, ps, bf), tr) ["reset", it]
    | null it = (display . Info) "environment reset" >> loop start
    | it `elem` ["v", "vars"] = (display . Info) "variables reset" >> loop ((zero, ps, bf), tr)
    | it `elem` ["p", "procs"] = (display . Info) "procedures reset" >> loop ((vs, [], bf), tr)
    | it `elem` ["b", "break"] = (display . Info) "break flag reset" >> loop ((vs, ps, False), tr)
    | it `elem` ["t", "trace"] = (display . Info) "trace reset" >> loop ((vs, ps, bf), [])
    | otherwise = throwError . Error $ "unrecognized aspect to reset: " ++ it
handleMeta env@(_, tr) ["trace"] =
    outputSection
        "Trace:"
        [ unlines $ zipWith (++) (idx : buf) ls
        | (i, s) <- zip [1 :: Int ..] tr
        , let
            len = length (show i) + 3
            idx = "#" ++ show i ++ space 2
            buf = repeat $ space len
            ls = lines $ prettify s
        ]
        >> loop env
handleMeta env@((vs, ps, bf), _) ["state"] = do
    outputSection
        "Variables:"
        -- invariant of IMP2.State.setVar guarantees no nempty string key
        [k ++ " = " ++ show v | (k, v) <- Map.toList vs, head k /= '_']
    outputSection "Procedures:" [prettify p | p <- ps]
    output $ "Break: " ++ show bf
    loop env
handleMeta (st, tr) ["load", it]
    | null it = throwError . Info $ "no filepath provided"
    | otherwise = do
        st' <- loadIMP st it
        loop (st', tr)
handleMeta env@(_, tr) ["write", it]
    | null it = throwError . Info $ "no filepath provided"
    | otherwise = writeIMP tr it >> loop env
handleMeta env@(_, tr) ["ast", it]
    | null it = throwError . Info $ "nothing to parse"
    | "#" <- it = throwError . Info $ "no parseIndex provided"
    | '#' : ds <- it = case readMaybe ds of
        Nothing -> throwError . ParseFail $ it
        Just i ->
            if i <= 0 || i > length tr
                then throwError $ Error $ "parseIndex out of bounds: " ++ show i
                else display (tr !! (i - 1)) >> loop env
    | otherwise = (liftIO . printAST) it >> loop env
handleMeta _ meta =
    throwError . Error $
        unlines
            [ "not a meta command: :" ++ unwords meta
            , "Enter :help to list available metacommands and :quit to exit."
            ]

-- | __TODO__
loadIMP :: State -> FilePath -> REPL State
loadIMP st path = do
    content <-
        (liftIO $ readFile path)
            `catchError` (\e -> throwError . IOFail $ unlines ["read from: " ++ path, show e])
    case parser path content of
        Left e -> throwError . ParseFail $ unlines [path, show e]
        Right stm -> do
            st' <- liftIMP $ interpret st stm
            display . Info $ "interpreted: " ++ path
            return st'

-- | __TODO__
writeIMP :: [Stm] -> FilePath -> REPL ()
writeIMP tr path = do
    let content = prettytrace tr
    _ <-
        (liftIO $ writeFile path content)
            `catchError` (\e -> throwError . IOFail $ unlines ["write to: " ++ path, show e])
    throwError . Info $ "wrote to: " ++ path

-- | __TODO__
printAST :: String -> IO ()
printAST input = case parser "ast" input of
    Left e -> print . ParseFail $ unlines [input, show e]
    Right (c :: Construct) -> print c

-- | __TODO__
prettytrace :: [Stm] -> String
prettytrace tr = prettify $ foldr Seq Skip tr

-- | __TODO__
clear :: REPL ()
clear = liftIO $ ANSI.clearScreen >> ANSI.setCursorPosition 0 0

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
