module IMP2.REPL where

import Config
import Control.Monad.IO.Class
import IMP2.Syntax
import Util

import qualified System.Console.Haskeline as Haskeline

import IMP2.Parser
import IMP2.Semantic.State

type Env = (State, [Stm])

type REPL a = Haskeline.InputT IO a

repl :: Haskeline.Settings IO -> Env -> IO ()
repl cfg env = do
    putStrLn welcome
    Haskeline.runInputT cfg $ loop env

loop :: Env -> REPL ()
loop env =
    do
        line <- Haskeline.getInputLine prompt
        case line of
            Nothing -> output goodbye
            Just "" -> loop env
            Just (':' : metas) -> undefined
            Just input -> case parser "interactive" input of
                Left err -> undefined -- throwError $ ParseFail $ unlines [input, show err]
                Right parsed -> dispatch env parsed >>= loop

dispatch :: Env -> Construct -> REPL Env
dispatch env@(st, tr) cnstr = case cnstr of
    Statement s -> undefined
    Arithmetic a -> undefined
    Boolean b -> undefined
    Whitespace -> return env

output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

display :: (Show a) => a -> REPL ()
display = output . show
