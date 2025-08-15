module IMP2.Rewrite where

import qualified Control.Monad.Trans.Except as Except
import qualified Data.Map as Map
import qualified System.Console.Haskeline as Haskeline

import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)

import IMP.Config
import IMP.Parser
import IMP.Syntax
import IMP.Util
import IMP2.Exception

----------------------------------------------------------------
-- State.hs

type Vars = Map.Map String Integer

type State = (Vars, [Proc], Bool)

type Conf = ([State], Maybe Stm)

initial :: State
initial = (Map.empty, [], False)

type IMP a = Except.ExceptT Exception IO a

----------------------------------------------------------------
-- Statement.hs

run :: (State, Stm) -> IMP State
run = undefined

step :: Conf -> IMP Conf
step = undefined

steps :: Conf -> IMP State
steps = undefined

interpret :: (State, Stm) -> IMP State
interpret (st, stm) =
    if small
        then run (st, stm)
        else steps ([st], Just stm)

----------------------------------------------------------------
-- REPL.hs
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

----------------------------------------------------------------
-- Util.hs

output :: String -> REPL ()
output msg = liftIO $ putStrLn msg >> flush

display :: (Show a) => a -> REPL ()
display = output . show
