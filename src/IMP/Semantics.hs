module IMP.Semantics where

import Control.Concurrent.Async (concurrently)
import Control.Exception (IOException, catch)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Random (randomIO)
import Text.Read (readMaybe)

import qualified Data.List as List
import qualified Data.Map as Map

import IMP.Syntax

type Vars = Map.Map Ident Val
type Procs = [Proc]
type State = (Vars, Procs, Bool)

initial :: State
initial = (Map.empty, [], False)

vrs :: State -> Vars
vrs (vars, _, _) = vars

prcs :: State -> Procs
prcs (_, procs, _) = procs

flg :: State -> Bool
flg (_, _, flag) = flag

getVar :: State -> Ident -> Val
getVar (vars, _, _) x = Map.findWithDefault 0 x vars

setVar :: State -> Ident -> Val -> State
setVar state "_" _ = state -- placeholder write-only variable
setVar (vars, procs, flag) x v = (Map.insert x v vars, procs, flag)

setVars :: State -> [(Ident, Val)] -> State
setVars = foldl (uncurry . setVar)

getProc :: State -> Ident -> Maybe Proc
getProc (_, procs, _) name = List.find (\p -> procname p == name) procs

setProc :: State -> Proc -> State
setProc (vars, procs, flag) def = (vars, def : procs, flag)

setFlag :: State -> State
setFlag (vars, procs, _) = (vars, procs, True)

resetFlag :: State -> State
resetFlag (vars, procs, _) = (vars, procs, False)

evalAexp :: State -> Aexp -> Val
evalAexp state aexp = case aexp of
    Numeral n -> n
    Variable x -> getVar state x
    Bin op e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case op of
                Add -> v1 + v2
                Sub -> v1 - v2
                Mul -> v1 * v2
    Time s -> countVarDefs state s

evalBexp :: State -> Bexp -> Bool
evalBexp state bexp = case bexp of
    Boolean b -> b
    Or b1 b2 -> evalBexp state b1 || evalBexp state b2
    And b1 b2 -> evalBexp state b1 && evalBexp state b2
    Not b -> not (evalBexp state b)
    Rel rop e1 e2 ->
        let
            v1 = evalAexp state e1
            v2 = evalAexp state e2
        in
            case rop of
                Eq -> v1 == v2
                Neq -> v1 /= v2
                Lt -> v1 < v2
                Leq -> v1 <= v2
                Gt -> v1 > v2
                Geq -> v1 >= v2

-- execStm :: State -> Stm -> IO (State, Bool)
execStm :: State -> Stm -> IO State
execStm state stm = case stm of
    Skip -> return state
    VarDef x e -> return $ setVar state x (evalAexp state e)
    Seq s1 s2 -> do
        state' <- execStm state s1
        execStm state' s2
    If b s1 s2 ->
        if evalBexp state b
            then execStm state s1
            else execStm state s2
    While b s ->
        if evalBexp state b && not (flg state)
            then do
                state' <- execStm state s
                execStm state' $ While b s
            else return $ if evalBexp state b then resetFlag state else state
    Print e -> print (evalAexp state e) >> return state
    Read x -> do
        result <- readInput $ x ++ " := "
        case result of
            Nothing -> do
                putStrLn ""
                exitSuccess
            Just input ->
                case readMaybe input of
                    Just value -> return $ setVar state x value
                    Nothing -> do
                        putStrLn "*** ERROR: invalid input, please enter an integer!"
                        execStm state (Read x) -- retry reading input
    Local x e s -> do
        let old = getVar state x
        let new = evalAexp state e
        let local = setVar state x new
        state' <- execStm local s
        return $ setVar state' x old
    NonDet s1 s2 -> do
        first <- (randomIO :: IO Bool)
        if first
            then execStm state s1
            else execStm state s2
    Par s1 s2 -> do
        ((vs1, ps1, flag'), (vs2, ps2, _)) <-
            concurrently
                (execStm state s1)
                (execStm state s2)
        -- NOTE: union is left-biased: on conflict vs1 (ps1) overrides vs2 (ps2)
        let vars' = Map.union vs1 vs2
        let procs' = List.union ps1 ps2
        return (vars', procs', flag')
    ProcDef def -> return $ setProc state def
    ProcInvoc name (arguments, returns) ->
        case getProc state name of
            Nothing -> do
                putStrLn $ "*** ERROR: undefined procedure: " ++ name
                return state
            Just (Proc _ (params, rets) body) -> do
                let vals = map (evalAexp state) arguments -- evaluate arguments
                let local = (Map.fromList (zip params vals), prcs state, flg state) -- into local state
                state' <- execStm local body -- execute body
                let rets' = zip returns $ map (getVar state') rets -- extract returns
                return $ setVars state rets' -- insert into callside
    Break -> return $ setFlag state
    Revert s b -> do
        let old = state
        new <- execStm state s
        if evalBexp new b
            then return old
            else return new

countVarDefs :: State -> Stm -> Val
countVarDefs state stm = case stm of
    Skip -> 0
    VarDef _ _ -> 1
    Seq s1 s2 -> countVarDefs state s1 + countVarDefs state s2
    If b s1 s2 ->
        if evalBexp state b
            then countVarDefs state s1
            else countVarDefs state s2
    While b s ->
        if evalBexp state b
            then countVarDefs state s
            else 0
    Print _ -> 0
    Read _ -> 1
    Local _ _ s -> countVarDefs state s + 1
    NonDet s1 s2 -> max (countVarDefs state s1) (countVarDefs state s2)
    Par s1 s2 -> countVarDefs state s1 + countVarDefs state s2
    ProcDef _ -> 0
    ProcInvoc name (args, _) -> case getProc state name of
        Just (Proc _ _ body) -> countVarDefs state body + toInteger (length args)
        Nothing -> 0
    Break -> 0
    Revert s b ->
        if evalBexp state b
            then countVarDefs state s
            else 0

readInput :: String -> IO (Maybe String)
readInput prompt = do
    putStr prompt
    hFlush stdout
    catch (Just <$> getLine) handleEOF
    where
        handleEOF :: IOException -> IO (Maybe String)
        handleEOF _ = return Nothing
