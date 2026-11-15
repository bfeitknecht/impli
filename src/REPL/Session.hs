{- |
Module      : REPL.Session
Description : Session of the IMP language REPL
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Session of the REPL: interpreter state, trace of executed statements and
settings, with rendering and reset of its aspects.
-}
module REPL.Session (
    Session (..),
    fresh,
    record,
    reset,
    render,
) where

import qualified Data.Map.Strict as Map

import IMP.Interpret
import IMP.Pretty
import IMP.State
import IMP.Syntax
import REPL.Command

-- | Session of the REPL.
data Session = Session
    { state :: State
    -- ^ Interpreter state.
    , trace :: [Stm]
    -- ^ Executed statements, most recent first.
    , semantics :: Semantics
    -- ^ Semantics to interpret statements with.
    }
    deriving (Eq, Show)

-- | Fresh session with initial state, empty trace and natural semantics.
fresh :: Semantics -> Session
fresh = Session initial []

-- | Record executed statement with resulting state.
record :: Stm -> State -> Session -> Session
record stm state' session = session {state = state', trace = stm : trace session}

-- | Reset aspect of session.
reset :: Aspect -> Session -> Session
reset aspect session = case aspect of
    All -> fresh (semantics session)
    Vars -> session {state = resetVars (state session)}
    Procs -> session {state = resetProcs (state session)}
    Trace -> session {trace = []}

-- | Render aspect of session.
render :: Aspect -> Session -> String
render aspect session = case aspect of
    All -> init . unlines $ map (`render` session) [Vars, Procs, Trace]
    Vars -> section "Variables:" [x ++ " = " ++ show v | (x, v) <- Map.toList (vars (state session)), visible x]
    Procs ->
        section
            "Procedures:"
            [prettify (ProcDef name params rets body) | (name, (params, rets, body)) <- Map.toList (procs (state session))]
    Trace ->
        section
            "Trace:"
            [ init . unlines $ zipWith (++) (index : repeat padding) (lines (prettify s))
            | (i, s) <- zip [1 :: Int ..] (reverse (trace session))
            , let
                index = '#' : show i ++ "  "
                padding = replicate (length index) ' '
            ]
    where
        section title [] = title ++ " none"
        section title entries = init . unlines $ title : map indented entries
        indented = init . unlines . map ("    " ++) . lines
