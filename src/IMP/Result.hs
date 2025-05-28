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

{- FOURMOLU_DISABLE -}
-- | Result type for variation over different possible outcomes.
data Result
    = ParseFail String  -- ^ Parse failure with error message.
    | AssFail String    -- ^ Assertion failure with an error message.
    | IOFail String     -- ^ IO failure with an error message.
    | Raised Integer    -- ^ Uncaught exception with the raised value.
    | Error String      -- ^ Generic error with message.
    | SigInt            -- ^ Interrupt signal.
    | Info String       -- ^ Informational message.
    | Ok                -- ^ Successful completion.
    deriving (Eq)
{- FOURMOLU_ENABLE -}

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
