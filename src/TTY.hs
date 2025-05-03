{-# LANGUAGE CPP #-}

module TTY (isTTY) where

#ifdef mingw32_HOST_OS
import System.Win32.Console (getConsoleMode)
import System.Win32.Handle (stdInputHandle)

isTTY :: IO Bool
isTTY = do
    result <- getConsoleMode stdInputHandle
    return $ result /= 0
#else
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

isTTY :: IO Bool
isTTY = queryTerminal stdInput
#endif
