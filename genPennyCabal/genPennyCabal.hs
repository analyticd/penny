module Main where

import Cartel
import Common
import Dependencies

pennyVer :: [Word]
pennyVer = [0,33,20161129,3]

pennyCopper :: Package
pennyCopper = exactly "penny-copper" [0,1,20161119,0]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions warnings
  , haskell2010
  , hsSourceDirs ["lib"]
  , otherExtensions ["TemplateHaskell"]
  ]

libraryDepends :: [Package]
libraryDepends =
  [ base
  , text
  , containers
  , time
  , transformers
  , bifunctors
  , rainbow
  , rainbox
  , semigroups
  , contravariant
  , bytestring
  , turtle
  , mtl
  , lens
  , async
  , pipes
  , pipesSafe
  , process
  , anonymousSums
  , void
  , pretty
  , managed
  , tasty
  , tastyHunit
  , monoidSubclasses
  , pinchot
  , earley
  , accuerr
  , ofx
  , parsec
  , timelens
  , prettyShow
  , templateHaskell
  , nonEmptySequence
  , optparseApplicative
  , pennyCopper
  ]

testDepends :: [Package]
testDepends = [ quickcheck, tasty, tastyQuickcheck, tastyTh, derive ]

-- | Creates test executables.  Generally these executables are meant
-- to be run by humans, rather than being automated.  This can be
-- useful for testing IO functions that would be difficult to test
-- automatically or with properties.
testExe
  :: [NonEmptyString]
  -- ^ Other modules
  -> FlagName
  -- ^ Test flag. Only build the executable if this flag is set.
  -> String
  -- ^ Name of test executable, such as "lessStream".  In that case,
  -- the module must be named "lessStream.hs".
  -> Section
testExe libMods flagName exeName = executable exeName $
  [ mainIs $ exeName ++ ".hs"
  , hsSourceDirs ["testExe"]
  , condBlock (flag flagName)
      ( buildable True
      , [ buildDepends $ testDepends ++ libraryDepends
        , otherModules libMods
        , ghcOptions ["-threaded"]
        ] ++ commonOptions
      )
      [ buildable False ]
  ]

props :: Properties
props = mempty
  { name = "penny"
  , version = pennyVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2012 - 2016 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/penny"
  , bugReports = "http://www.github.com/massysett/penny/issues"
  , synopsis = "Extensible double-entry accounting system"
  , extraSourceFiles = ["README.md"]
  , dataFiles = []
  , description =
    [ "Penny is a double-entry accounting system."
    , ""
    , "For more information, please see the README.md file, which"
    , "is available in the source tarball or is visible at the bottom"
    , "of the Penny homepage:"
    , ""
    , "<http://www.github.com/massysett/penny>"
    ]
  , category = "Console, Finance"
  }

-- | Names of all tests in the @testExe@ directory.  (Currently it is
-- not easy to read these in dynamically.)
testNames :: [String]
testNames =
  [ "doNothing"
  , "testLess"
  ]

main :: IO ()
main = defaultMain $ do
  libMods <- modules "../penny/lib"
  testFlag <- makeFlag "testExe" $ FlagOpts
    { flagDescription = "Build human-operated tests"
    , flagDefault = False
    , flagManual = True
    }
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny" ]
      ++ fmap (testExe libMods testFlag) testNames
    )
