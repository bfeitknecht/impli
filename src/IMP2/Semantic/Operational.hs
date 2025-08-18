{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module IMP2.Semantic.Operational where

import IMP2.Expression
import IMP2.State
import IMP2.Syntax

import Control.Monad.IO.Class

step :: ([State], Stm) -> IMP Conf
step (stack, Skip) = return (stack, Nothing)
step (state : states, VarDef x f a) =
    let
        v = getVar state x
        v' = evaluate state a
        state' = setVar state x $ case f of
            Def -> v'
            Inc -> v + v'
            Dec -> v - v'
            Prod -> v * v'
            Quot -> v // v'
            Rem -> v %% v'
    in
        return (state' : states, Nothing)
step (stack, Seq s1 s2) = do
    (stack', rest) <- step (stack, s1)
    case rest of
        Nothing -> return (stack', Just s2)
        Just s1' -> return (stack', Just $ Seq s1' s2)
step (stack@(state : _), IfElse b s1 s2) =
    if evaluate state b
        then return (stack, Just s1)
        else return (stack, Just s2)
step (stack@(state : states), While b s) =
    if evaluate state b
        then
            if not $ getBreak state
                then return (stack, Just $ Seq s $ While b s)
                else return (resetBreak state : states, Nothing)
        else return (stack, Nothing)
step (stack, Print a) = liftIO (print $ evaluate (head stack) a) >> return (stack, Nothing)
step (state : states, Read x) = do
    v <- getVal $ x ++ " := "
    return (setVar state x v : states, Nothing)
step _ = undefined

{-
Local x e s -> do
    let
        snapshot = ([(x, getVar state x)], prs state, brk state)
        local = setVar state x $ evaluate state e
    return (local : states, Just $ Seq s $ Restore snapshot)
Par s1 s2 -> do
    left <- randomIO :: REPL Bool
    if left
        then do
            (stack', rest1) <- step stack s1
            case rest1 of
                Nothing -> return (stack', Just s2)
                Just s1' -> return (stack', Just $ Par s1' s2)
        else do
            (stack', rest2) <- step stack s2
            case rest2 of
                Nothing -> return (stack', Just s1)
                Just s2' -> return (stack', Just $ Par s1 s2')
NonDet s1 s2 -> do
    left <- randomIO :: REPL Bool
    if left
        then return (stack, Just s1)
        else return (stack, Just s2)
ProcDef p -> return (setProc state p : states, Nothing)
ProcInvoc name (arguments, returns) -> do
    case getProc state name of
        Nothing -> throwError $ Error $ "undefined procedure: " ++ name
        Just (Procedure _ (params, rets) body)
            | length arguments /= length params -> throwError $ Error "mismatched number of arguments"
            | length returns /= length rets -> throwError $ Error "mismatched number of return values"
            | otherwise -> do
                let
                    vals = map (evaluate state) arguments -- evaluate arguments
                    local = (Map.fromList (zip params vals), prs state, brk state) -- into local state
                return (local : stack, Just $ Seq body $ Return rets returns)
Restore (vars, procs, flag) -> do
    let state' = setVars (vrs state, procs, flag) vars
    return (state' : states, Nothing)
Return rets returns -> case stack of
    (callee : caller : rest) -> do
        let
            vals = map (getVar callee) rets
            caller' = setVars caller $ zip returns vals
        return (caller' : rest, Nothing)
    _ -> throwError $ Error "insufficient callstack!"
Break -> return (setBreak state : states, Nothing)
Revert s b -> do
    -- uninitialized variables can't be restored!
    let snapshot = (Map.toList (vrs state), prs state, brk state)
    return (stack, Just $ Seq s $ If b (Restore snapshot) Skip)
Match e ms d -> do
    let v = evaluate state e
    case lookup v ms of
        Just s -> return (stack, Just s)
        Nothing -> return (stack, Just d)
Havoc x -> do
    v <- randomIO :: REPL Integer
    return (setVar state x v : states, Nothing)
Assert b ->
    if evaluate state b
        then return (stack, Nothing)
        else throwError $ AssFail $ prettify b
Flip i s1 s2 ->
    if getFlip state i
        then return (setFlop state i : states, Just s1)
        else return (setFlip state i : states, Just s2)
Raise e -> throwError $ Raised $ evaluate state e
Try s1 x s2 -> do
    (stack', rest) <- catchError (step stack s1) $ \err -> case err of
        Raised v -> do
            let state' = setVar state x v
            return (state' : states, Just s2)
        _ -> throwError err
    case rest of
        Nothing -> return (stack', Nothing)
        Just s1' -> return (stack', Just $ Try s1' x s2)
Swap x y -> do
    let
        v = getVar state x
        w = getVar state y
    return (setVars state [(x, w), (y, v)] : states, Nothing)
Timeout s e ->
    if evaluate state e <= 0
        then return (stack, Nothing)
        else do
            (stack', rest) <- step stack s
            case rest of
                Nothing -> return (stack', Nothing)
                Just s' -> return (stack', Just $ Timeout s' $ e - 1)
Alternate s1 s2 -> do
    (stack', rest) <- step [state] s1
    case rest of
        Just s1' -> return (stack', Just $ Alternate s2 s1')
        Nothing -> return (stack', Just s2)
-}

steps :: ([State], Stm) -> IMP State
steps ([], _) = error ""
steps conf = do
    (stack', rest) <- step conf
    case rest of
        Nothing -> return . head $ stack'
        Just stm -> steps (stack', stm)
