{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_syllable (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "syllable"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Haskell library for syllable separation"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
