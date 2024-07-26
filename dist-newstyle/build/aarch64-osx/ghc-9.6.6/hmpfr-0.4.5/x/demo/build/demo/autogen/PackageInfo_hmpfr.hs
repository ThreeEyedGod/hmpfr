{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hmpfr (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hmpfr"
version :: Version
version = Version [0,4,5] []

synopsis :: String
synopsis = "Haskell binding to the MPFR library"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/michalkonecny/hmpfr"
