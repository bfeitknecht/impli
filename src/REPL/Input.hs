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
import Data.List (isPrefixOf, nub, sort)
import Text.Parsec.Error (Message (SysUnExpect), errorMessages)

import qualified Data.Map.Strict as Map

import IMP.Lexer (keywords)
import IMP.Parser
import IMP.State
import IMP.Syntax
import REPL.Command (commands)
import REPL.Session

-- | Whether input is an unfinished construct, i.e. fails to parse only because it ended early.
incomplete :: String -> Bool
incomplete input = case dropWhile isSpace input of
    "" -> False
    ':' : _ -> False -- INFO: Metacommands are single line
    _ -> case parser @Construct "input" input of
        Right _ -> False
        Left e -> SysUnExpect "" `elem` errorMessages e

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
