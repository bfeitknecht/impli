{- |
Module      : IMP.Result
Description : Defines the result types for the IMP interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

This module defines the result type used throughout the IMP interpreter.
These result types are used by the "IMP.REPL" and "IMP.Semantics.Statement"
modules to report various outcomes of interpreter operations to users.
-}
module IMP.Result (
    Result (..),
) where

-- | Result type for variation over different possible outcomes.
--    Used by "IMP.REPL" and "IMP.Semantics.Statement" to indicate success,
--    failure, or other execution states.
{- FOURMOLU_DISABLE -}
data Result
    = ParseFail String  -- ^ Parse failure with message from "IMP.Parser".
    | AssFail String    -- ^ Assertion failure with message, triggered by @assert@ statements.
    | IOFail String     -- ^ IO failure with message, for IO operations in "IMP.REPL".
    | Raised Integer    -- ^ Exception with the raised value, thrown by @raise@ statements.
    | Error String      -- ^ Generic error with message, for other runtime errors.
    | SigInt            -- ^ Interrupt signal, when execution is forcibly terminated.
    | Info String       -- ^ Generic information with message, for user feedback.
    | Ok                -- ^ Successful completion of execution.
    deriving (Eq)
{- FOURMOLU_ENABLE -}

-- | Show instance for 'Result', providing formatted error messages for display in "IMP.REPL".
instance Show Result where
    show err = case err of
        ParseFail msg -> "*** ERROR: parse failure in: " ++ msg
        AssFail msg -> "*** ERROR: assertion failure in: " ++ msg
        IOFail msg -> "*** ERROR: IO failure in: " ++ msg
        Raised val -> "*** ERROR: uncaught exception raised with: " ++ show val
        Error msg -> "*** ERROR: " ++ msg
        SigInt -> "*** ERROR: execution interrupted"
        Info msg -> "+++ INFO: " ++ msg
        Ok -> ""
