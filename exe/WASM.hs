module Main where

import IMP.State
import IMP.Statement
import IMP.Syntax

import qualified Control.Monad.Trans.Except as Except

main :: IO ()
main =
    let stm = VarDef "x" Def (Val 1) <> Print (Var "x")
    in Except.runExceptT (interpret (stm, initial)) >>= either print print
