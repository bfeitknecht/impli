module Util where

import System.IO

-- | Flush stdout.
flush :: IO ()
flush = hFlush stdout
