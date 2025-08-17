{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module IMP2.REPL where

import qualified Control.Monad.Trans.Except as Except
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as Haskeline

import System.Exit
import System.IO

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import Config
import IMP2.Exception
import IMP2.Parser
import IMP2.Semantic.Expression
import IMP2.Semantic.State
import IMP2.Semantic.Statement
import IMP2.Syntax

type Env = (State, [Stm])

type REPL = Except.ExceptT Exception (Haskeline.InputT IO)

-- | Lift IMP computation into  REPL monad.
liftIMP :: IMP a -> REPL (Either Exception a)
liftIMP = liftIO . Except.runExceptT

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
        Nothing -> output goodbye
        Just "" -> loop env
        Just (':' : rest) -> handleMeta . normalizeMeta $ words rest
        Just input ->
            either
                (\e -> (display . ParseFail . unlines) [input, show e] >> loop env)
                (\c -> dispatch env c >>= loop)
                (parser "interactive" input)

-- | __TODO__
dispatch :: Env -> Construct -> REPL Env
dispatch env@(st, tr) cnstr = case cnstr of
    Statement stm -> do
        result <- liftIMP (interpret (st, stm))
        either
            ( \e -> case e of
                AssertFail _ -> display e >> return env
                Raised _ -> display e >> return env
                -- _ -> throwError e
            )
            (\st' -> return (st', stm : tr))
            result
    Arithmetic aexp -> display (evaluate st aexp) >> return env
    Boolean bexp -> output (if evaluate st bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | __TODO__
output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

-- | __TODO__
display :: (Show a) => a -> REPL ()
display = output . show

-- | Flush stdout.
flush :: IO ()
flush = hFlush stdout

normalizeMeta :: [String] -> [String]
normalizeMeta = undefined

handleMeta :: [String] -> REPL ()
handleMeta = undefined
