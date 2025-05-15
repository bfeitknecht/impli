{- |
Module      : IMP.Result
Description : Defines the result types for the IMP interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the result type used throughout the IMP interpreter.
-}
module IMP.Result where

data Result
    = ParseFail String
    | AssFail String
    | IOFail String
    | Raised Integer
    | Info String
    | Error String
    | Ok
    deriving (Eq)

instance Show Result where
    show err = case err of
        ParseFail msg -> "*** ERROR: parse failure in: " ++ msg
        AssFail msg -> "*** ERROR: assertion failure in: " ++ msg
        IOFail msg -> "*** ERROR: IO failure in: " ++ msg
        Raised val -> "*** ERROR: uncaught exception raised with: " ++ show val
        Ok -> ""
        Info msg -> "+++ INFO: " ++ msg
        Error msg -> "*** ERROR: " ++ msg
