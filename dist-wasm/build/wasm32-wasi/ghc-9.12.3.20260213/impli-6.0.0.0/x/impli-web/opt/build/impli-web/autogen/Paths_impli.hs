{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_impli (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [6,0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/bf/.ghc-wasm/.cabal/bin"
libdir     = "/Users/bf/.ghc-wasm/.cabal/lib/wasm32-wasi-ghc-9.12.3.20260213-inplace/impli-6.0.0.0-inplace-impli-web"
dynlibdir  = "/Users/bf/.ghc-wasm/.cabal/lib/wasm32-wasi-ghc-9.12.3.20260213-inplace"
datadir    = "/Users/bf/.ghc-wasm/.cabal/share/wasm32-wasi-ghc-9.12.3.20260213-inplace/impli-6.0.0.0"
libexecdir = "/Users/bf/.ghc-wasm/.cabal/libexec/wasm32-wasi-ghc-9.12.3.20260213-inplace/impli-6.0.0.0"
sysconfdir = "/Users/bf/.ghc-wasm/.cabal/etc"

getBinDir     = catchIO (getEnv "impli_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "impli_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "impli_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "impli_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "impli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "impli_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
