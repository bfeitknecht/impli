{- |
Module      : IMP.Exception
Description : Exceptions for the IMP language interpreter
Copyright   : (c) Basil Feitknecht, 2025
License     : MIT
Maintainer  : bfeitknecht@ethz.ch
Stability   : stable
Portability : portable

TODO
-}
module IMP.Exception (
    Exception (..),
) where

-- | Exception type that encapsulates possible problems.
{- FOURMOLU_DISABLE -}
data Exception
    = ParseFail String -- ^ Parse failure, probably caused by 'IMP.Parser.parser'.
    | AssertFail String -- ^ Assertion failure during execution.
    | IOFail String -- ^ IO failure.
    | Raised Integer -- ^ Value raised during execution.
    | Error String -- ^ Generic error.
    | Info String -- ^ Generic information.
    | Empty -- ^ Read aborted.
    deriving (Eq)
{- FOURMOLU_ENABLE -}

-- | Instance of 'Show' with severity marker followed by content.
instance Show Exception where
    show e = case e of
        ParseFail msg -> "*** ERROR: parse failure in: " ++ msg
        AssertFail msg -> "*** ERROR: assertion failure in: " ++ msg
        IOFail msg -> "*** ERROR: IO failure in: " ++ msg
        Raised val -> "*** ERROR: uncaught exception raised with: " ++ show val
        Error msg -> "*** ERROR: " ++ msg
        Info msg -> "+++ INFO: " ++ msg
        Empty -> ""
