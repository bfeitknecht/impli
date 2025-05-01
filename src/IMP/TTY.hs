{-# LANGUAGE CPP #-}

module IMP.TTY (isTTY) where

#ifdef mingw32_HOST_OS
import System.Win32.Console (getConsoleMode, stdinHandle)

isTTY :: IO Bool
isTTY = do
    handle <- stdinHandle
    result <- getConsoleMode handle
    return $ result /= 0
#else
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

isTTY :: IO Bool
isTTY = queryTerminal stdInput
#endif
