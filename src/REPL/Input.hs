{-# LANGUAGE TypeApplications #-}

{- |
Module      : REPL.Input
Description : Input handling of the IMP language REPL
Copyright   : (c) Basil Feitknecht, 2026
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

Line-editing support shared by the frontends: whether input continues on
the next line, and completion candidates for a word.
-}
module REPL.Input (
    incomplete,
    completions,
) where

import Data.Char (isSpace)
import Data.Either (lefts)
import Data.List (isPrefixOf, nub, sort)
import Text.Parsec.Error (errorPos)
import Text.Parsec.Pos (initialPos, updatePosString)

import qualified Data.Map.Strict as Map

import IMP.Lexer (keywords)
import IMP.Parser
import IMP.State
import IMP.Syntax
import REPL.Command (commands)
import REPL.Session

-- | Whether input is an unfinished construct, i.e. fails to parse only at its end.
incomplete :: String -> Bool
incomplete input = case dropWhile isSpace input of
    "" -> False
    ':' : _ -> False -- INFO: Metacommands are single line
    _ -> case lefts results of
        errors
            | length errors < length results -> False -- INFO: Some construct parses
            | otherwise -> any ((>= end) . errorPos) errors
    where
        end = updatePosString (initialPos "input") input
        results =
            [ () <$ parser @Stm "input" input
            , () <$ parser @Aexp "input" input
            , () <$ parser @Bexp "input" input
            , () <$ parser @Construct "input" input
            ]

-- | Completion candidates for word prefix: keywords, metacommands, variables and procedures.
completions :: Session -> String -> [String]
completions session prefix = sort . nub $ filter (prefix `isPrefixOf`) candidates
    where
        candidates
            | ":" `isPrefixOf` prefix = commands
            | otherwise =
                filter ((> 1) . length) keywords
                    ++ filter visible (Map.keys (vars (state session)))
                    ++ Map.keys (procs (state session))
