module Main where

import qualified Cartel as A
import qualified CartelCommon as C

libraryDepends :: [A.Package]
libraryDepends =
  [ C.base
  , C.penny
  , C.quickcheck
  , C.containers
  , C.dekaTests
  , C.barecheck
  , C.anonymous_sums
  , C.anonymous_sums_tests
  , C.time
  ]

properties :: A.Properties
properties = C.baseProperties
  { A.prName = A.prName C.baseProperties ++ "-tests"
  , A.prSynopsis = A.prSynopsis C.baseProperties
      ++ " - tests"
  , A.prDescription = A.prDescription C.baseProperties
      ++ [ ""
         , "This package contains a library with QuickCheck"
         , "generators, and executables to test the main"
         , "Penny library."
         ]
  }

library
  :: [String]
  -- ^ Test library modules
  -> A.Library
library ms = A.Library $
  [ A.LibExposedModules ms
  , A.LibExposed True
  , A.hsSourceDirs ["lib"]
  , A.buildDepends libraryDepends
  ]

  ++ C.commonBuildInfo

cabal
  :: [String]
  -- ^ Library modules
  -> A.Cabal
cabal libMods = A.empty
  { A.cProperties = properties
  , A.cRepositories = [C.repo]
  , A.cLibrary = Just (library libMods)
  }

main :: IO ()
main = do
  libMods <- A.modules "lib"
  A.render "genCabal.hs" $ cabal libMods
