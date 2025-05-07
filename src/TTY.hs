{-# LANGUAGE CPP #-}

module TTY (isTTY) where

#ifdef mingw32_HOST_OS
import System.Win32.Console (getConsoleMode)
import System.Win32.Types (HANDLE)
import System.Win32 (getStdHandle, sTD_INPUT_HANDLE)

isTTY :: IO Bool
isTTY = do
    h <- getStdHandle sTD_INPUT_HANDLE
    result <- getConsoleMode h
    return $ result /= 0

#else
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

isTTY :: IO Bool
isTTY = queryTerminal stdInput
#endif
