{-# LANGUAGE TypeApplications #-}

{- |
Module      : WASM
Description : TODO
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module Main where

import Config

import IMP.Exception
import IMP.Expression
import IMP.Parser
import IMP.State
import IMP.Statement
import IMP.Syntax

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)

import qualified Control.Monad.Trans.Except as Except

import System.IO (
    BufferMode (NoBuffering),
    hFlush,
    hSetBuffering,
    stdin,
    stdout,
 )

-- | TODO
test :: IO ()
test =
    let stm = VarDef "x" Def (Val 1) <> Print (Var "x")
    in Except.runExceptT (interpret (stm, initial)) >>= either print print

-- TODO
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    putStrLn welcome
    Except.runExceptT (loop start) >>= either (\e -> print e) (\_ -> putStrLn goodbye)
    main

-- | TODO
type Env = ([Stm], State)

-- | TODO
start :: Env
start = ([], initial)

-- | TODO
loop :: Env -> IMP ()
loop env =
    do
        line <- liftIO $ putStr prompt >> hFlush stdout >> getLine
        case line of
            "" -> loop env
            input ->
                either
                    (\e -> throwError . ParseFail $ unlines [input, show e])
                    (\c -> dispatch env c >>= loop)
                    (parser "interactive" input)
        `catchError` \e -> case e of
            Empty -> output "" -- ctrl-d during read, flush line and exit cleanly
            AssertFail _ -> throwError e -- unrecoverable, propagate
            Raised _ -> throwError e -- ''
            _ -> display e >> loop env -- mistakes happen

-- | TODO
dispatch :: Env -> Construct -> IMP Env
dispatch env@(trace, state) cnstr = case cnstr of
    Statement stm -> do
        state' <- interpret (stm, state)
        return (stm : trace, state')
    Arithmetic aexp -> display (evaluate state aexp) >> return env
    Boolean bexp -> output (if evaluate state bexp then "true" else "false") >> return env
    Whitespace -> return env

-- | TODO
output :: String -> IMP ()
output = liftIO . putStrLn

-- | TODO
display :: (Show a) => a -> IMP ()
display = output . show
