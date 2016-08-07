module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,0,0]

atleast :: NonEmptyString -> Version -> Package
atleast n v = package n (gtEq v)

-- Packages

base :: Package
base = closedOpen "base" [4,8] [5]

pinchot :: Package
pinchot = atleast "pinchot" [0,18,2,0]

earley :: Package
earley = atleast "Earley" [0,10,1,0]

containers :: Package
containers = atleast "containers" [0,5,7]

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
  , earley
  , containers
  ]

props :: Properties
props = blank
  { name = "penny-copper"
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
  , description =
    [ "Penny is a double-entry accounting system."
    , ""
    , "This package contains the grammar for ledger files."
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
  libMods <- modules "../penny-copper/lib"
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny"
      ]
    )
