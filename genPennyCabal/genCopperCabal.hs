module Main where

import Cartel
import Dependencies

copperVer :: [Word]
copperVer = [0,1,0,0]

-- Packages

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-W"]
  , haskell2010
  , hsSourceDirs ["lib"]
  , otherExtensions ["TemplateHaskell"]
  ]

libraryDepends :: [Package]
libraryDepends =
  [ base
  , pinchot
  , containers
  , lens
  , earley
  ]

testDepends :: [Package]
testDepends = [ quickcheck, tasty, tastyQuickcheck, tastyTh, derive ]

props :: Properties
props = mempty
  { name = "penny-copper"
  , version = copperVer
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
    , ""
    , "This package contains only the types needed to parse ledger files."
    ]
  , category = "Console, Finance"
  }

main :: IO ()
main = defaultMain $ do
  libMods <- modules "../penny-copper/lib"
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny" ]
    )
