module IMP2.Semantic.Operational where

import IMP2.Syntax

import IMP2.Semantic.State

step :: Conf -> IMP Conf
step = undefined

steps :: Conf -> IMP State
steps ([], _) = error ""
steps conf = do
    ((st' : sts'), rest) <- step conf
    case rest of
        Nothing -> return st'
        _ -> steps ((st' : sts'), rest)
