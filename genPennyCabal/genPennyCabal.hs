module Main where

import Cartel
import Common
import Dependencies

pennyVer :: [Word]
pennyVer = [0,33,0,0]

pennyCopper :: Package
pennyCopper = exactly "penny-copper" [0,1,0,0]

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

props :: Properties
props = mempty
  { name = "penny"
  , version = pennyVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2012 - 2015 Omari Norman"
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

main :: IO ()
main = defaultMain $ do
  libMods <- modules "../penny/lib"
  -- testMods <- modules "tests"
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny" ]
{-
      , testSuite "penny-properties" $
        exitcodeFields "penny-properties.hs" ++
        commonOptions ++
        [ hsSourceDirs ["properties"]
        , buildDepends libraryDepends
        , buildDepends testDepends
        , otherModules libMods
        ]
-}
    )
