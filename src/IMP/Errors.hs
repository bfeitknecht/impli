module IMP.Errors where

import Control.Exception (try)

data Errors
    = ParseError String
    | ExecError String
    | IOError String
    deriving (Eq, Show)

type Result a = Either Errors a

msg :: Errors -> String
msg (ParseError m) = m
msg (ExecError m) = m
msg (IOError m) = m

report :: Errors -> IO ()
report err = putStrLn $ "*** ERROR: " ++ msg err

fromException :: IOError -> Errors
fromException e = IOError (show e)

handleIO :: IO a -> IO (Result a)
handleIO action = do
    result <- try action
    return $ case result of
        Left e -> Left (fromException e)
        Right value -> Right value

combineErrors :: [Errors] -> Errors
combineErrors errs = ExecError $ unlines (map show errs)

fromMaybe :: Errors -> Maybe a -> Result a
fromMaybe err Nothing = Left err
fromMaybe _ (Just value) = Right value
