module IMP2.Exception (
    Exception (..),
) where

-- | __TODO__
{- FOURMOLU_DISABLE -}
data Exception
    = ParseFail String
    | AssertFail String
    | IOFail String
    | Raised Integer
    | Error String
    | Info String
    | Empty
    deriving (Eq)
{- FOURMOLU_ENABLE -}

-- | __TODO__
instance Show Exception where
    show err = case err of
        ParseFail msg -> "*** ERROR: parse failure in: " ++ msg
        AssertFail msg -> "*** ERROR: assertion failure in: " ++ msg
        IOFail msg -> "*** ERROR: IO failure in: " ++ msg
        Raised val -> "*** ERROR: uncaught exception raised with: " ++ show val
        Error msg -> "*** ERROR: " ++ msg
        Info msg -> "+++ INFO: " ++ msg
        Empty -> ""
