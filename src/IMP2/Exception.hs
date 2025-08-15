module IMP2.Exception (
    Exception (..),
) where

-- | Exception type capturing possible problems.
{- FOURMOLU_DISABLE -}
data Exception
    = ParseFail String      -- ^ Parse failure with message from "IMP.Parser".
    | AssertFail String     -- ^ Assertion failure with message, triggered by @assert@ statements.
    | IOFail String         -- ^ IO failure with message, for IO operations in "IMP.REPL".
    | Raised Integer        -- ^ Exception with the raised value, thrown by @raise@ statements.
    | Error String          -- ^ Generic error with message, for other runtime errors.
    | Info String           -- ^ Generic information with message, for user feedback.
    | Empty                 -- ^
    deriving (Eq)
{- FOURMOLU_ENABLE -}

-- | Show instance for 'Result', providing formatted error messages for display in "IMP.REPL".
instance Show Exception where
    show err = case err of
        ParseFail msg -> "*** ERROR: parse failure in: " ++ msg
        AssertFail msg -> "*** ERROR: assertion failure in: " ++ msg
        IOFail msg -> "*** ERROR: IO failure in: " ++ msg
        Raised val -> "*** ERROR: uncaught exception raised with: " ++ show val
        Error msg -> "*** ERROR: " ++ msg
        Info msg -> "+++ INFO: " ++ msg
        Empty -> ""
