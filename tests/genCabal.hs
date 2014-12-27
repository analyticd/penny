module Main where

import qualified Cartel as C
import CartelCommon

properties :: C.Properties
properties = baseProperties
  { C.prDescription = C.prDescription baseProperties
      ++ [ ""
         , "This package contains a library and executables for testing."
         ]
  , C.prName = "penny-tests"
  }

quickCheck :: C.Package
quickCheck = C.closedOpen "QuickCheck" [2,7] [2,8]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,4] [0,5]

depends :: [C.Package]
depends =
  [ base
  , quickCheck
  , quickpull
  , penny
  ]

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library $
  [ C.LibExposedModules ms
  , C.LibExposed True
  , C.hsSourceDirs ["lib"]
  , C.buildDepends depends
  ] ++ commonOptions

tests
  :: [String]
  -- ^ Test program modules
  -> [String]
  -- ^ Library modules
  -> C.TestSuite
tests ts ms = C.TestSuite "penny-properties" $
  [ C.TestType C.ExitcodeStdio
  , C.TestMainIs "penny-properties.hs"
  , C.hsSourceDirs ["progs", "lib"]
  , C.buildDepends depends
  ] ++ commonOptions

cabal
  :: [String]
  -- ^ Test program modules
  -> [String]
  -- ^ Library modules
  -> C.Cabal
cabal testMods libMods = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just (library libMods)
  , C.cFlags = [debug]
  , C.cTestSuites = [tests testMods libMods]
  }

main :: IO ()
main = do
  libMods <- C.modules "lib"
  testMods <- C.modules "progs"
  C.render "genCabal.hs" $ cabal testMods libMods
