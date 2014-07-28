-- | Uses Cartel to generate the Cabal file.  Written for Cartel
-- version 0.10.0.0.
--
-- Currently generates a Cabal file that does NOT use the cabal
-- feature that allows executables to depend on the library
-- contained in the same package.  The "cabal haddock --executables"
-- command is having trouble with this feature:
--
-- https://github.com/haskell/cabal/issues/1919
--
-- By not depending on the library, the build times go up.  Also,
-- this introduces redundancy to the Cabal file, but since Cartel
-- handles that well it's not such a problem.

module Main (main) where

import qualified Cartel as A
import Prelude hiding (either)
import CartelCommon

commonOptions :: A.Field a => [a]
commonOptions = cond : commonBuildInfo
  where
    cond = A.cif (A.flag "debug")
      [ (A.ghcOptions ["-auto-all", "-caf-all", "-rtsopts"]) ]
      []

flags :: [A.Flag]
flags =
  [ A.Flag "debug" "turns on debugging options" False True
  , manualFlag "build-penny-gibberish" "Build the penny-gibberish executable"
      False
  ]

libraryDepends :: [A.Package]
libraryDepends =
  [ base
  , bifunctors
  , bytestring
  , containers
  , deka
  , old_locale
  , parsec
  , split
  , text
  , time
  , transformers
  , action_permutations
  , cereal
  , contravariant
  , either
  , semigroups
  , anonymous_sums
  , matchers
  , multiarg
  , ofx
  , prednote
  , rainbow
  , rainbox
  ]

extraSourceFiles :: [String]
extraSourceFiles =
  [ "install-docs"
  , "README.md"
  , "doc/*.dot"
  , "doc/*.hs"
  , "doc/examples/*.pny"
  , "doc/man/*.1"
  , "doc/man/*.7"
  , "current-versions.txt"
  , "minimum-versions.txt"
  , "changelog"
  , "genCabal.hs"
  ]

properties :: A.Properties
properties = baseProperties
  { A.prExtraSourceFiles = extraSourceFiles
  }

library
  :: [String]
  -- ^ Library modules
  -> A.Library
library ms = A.Library $
  [ A.LibExposedModules ms
  , A.otherModules ["Paths_penny"]
  , A.LibExposed True
  , A.hsSourceDirs ["lib"]
  , A.buildDepends libraryDepends
  ]

  ++ commonOptions

executable
  :: [String]
  -- ^ Library modules
  -> String
  -- ^ Executable name
  -> String
  -- ^ Main-is file
  -> (A.Flag, A.Executable)
executable libMods n mi = (fl, ex)
  where
    ex = A.Executable n $
      [ A.ExeMainIs $ mi ++ ".hs"
      , A.otherModules $ "Paths_penny" : libMods
      , A.hsSourceDirs ["bin", "lib"]
      , A.buildDepends libraryDepends
      , A.cif (A.flag ("build-" ++ n))
          [ A.buildable True ]
          [ A.buildable False ]
      ] ++ commonOptions

    fl = manualFlag ("build-" ++ n)
      ("build the " ++ n ++ " executable") True

cabal
  :: [String]
  -- ^ Library modules
  -> A.Cabal
cabal libMods = A.empty
  { A.cProperties = properties
  , A.cRepositories = [repo]
  , A.cFlags = flags ++ flgs
  , A.cLibrary = Just (library libMods)
  --, A.cExecutables = pennyGibberish libMods testMods : exes
  , A.cExecutables = []
  }
  where
    (flgs, exes) = unzip . map (uncurry (executable libMods)) $
      ("penny", "penny-main") : map same ["penny-selloff",
        "penny-diff", "penny-reprint", "penny-reconcile" ]
    same x = (x, x)

main :: IO ()
main = do
  libMods <- A.modules "lib"
  let c = cabal libMods
  A.render "genCabal.hs" c
