{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_impli (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "impli"
version :: Version
version = Version [6,0,0,0] []

synopsis :: String
synopsis = "Interpreter for an imperative toy language"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/bfeitknecht/impli"
