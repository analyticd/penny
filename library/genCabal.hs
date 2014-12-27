module Main where

import qualified Cartel as C

import CartelCommon

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library $
  [ C.LibExposedModules ms
  , C.otherModules ["Paths_penny"]
  , C.LibExposed True
  , C.hsSourceDirs ["lib"]
  , C.buildDepends libraryDepends
  ] ++ commonOptions

cabal
  :: [String]
  -- ^ Library modules
  -> C.Cabal
cabal libMods = C.empty
  { C.cProperties = baseProperties
  , C.cRepositories = [repo]
  , C.cLibrary = Just (library libMods)
  , C.cFlags = [debug]
  }

main :: IO ()
main = do
  ms <- C.modules "lib"
  C.render "genCabal.hs" $ cabal ms
