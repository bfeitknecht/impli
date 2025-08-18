{-# OPTIONS_GHC -Wno-x-partial #-}

module IMP2.Semantic.Operational where

import IMP2.State
import IMP2.Syntax

step :: Conf -> IMP Conf
step (stack, Just Skip) = return (stack, Nothing)

steps :: Conf -> IMP State
steps ([], _) = error ""
steps conf = do
    (stack', rest) <- step conf
    if rest == Nothing
        then return . head $ stack'
        else steps (stack', rest)
