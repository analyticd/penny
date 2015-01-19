-- | This file written for Cartel version 0.12.0.0

module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,0,0]

-- Packages

base :: Package
base = closedOpen "base" [4,7,0,0] [4,8]

text :: Package
text = closedOpen "text" [1,2] [1,3]

containers :: Package
containers = closedOpen "containers" [0,5] [0,6]

parsec :: Package
parsec = closedOpen "parsec" [3,1,7] [3,2]

time :: Package
time = closedOpen "time" [1,4] [1,6]

transformers :: Package
transformers = closedOpen "transformers" [0,3] [0,5]

prednote :: Package
prednote = closedOpen "prednote" [0,28] [0,29]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-Wall"]
  , haskell2010
  ]

libraryDepends :: [Package]
libraryDepends =
  [ base
  , text
  , containers
  , parsec
  , time
  , transformers
  , prednote
  ]

props :: Properties
props = blank
  { name = "penny"
  , version = pennyVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2012 - 2014 Omari Norman"
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
    , "For more information, please see the README.md file, which"
    , "is available in the source tarball or is visible at the bottom"
    , "of the Penny homepage:"
    , ""
    , "<http://www.github.com/massysett/penny>"
    ]
  , category = "Console, Finance"
  , testedWith = let f v = (ghc, eq v) in map f [[7,6,2], [7,8,3]]
  }

main :: IO ()
main = defaultMain $ do
  libMods <- modules "lib"
  return
    ( props
    ,   exposedModules libMods
      : hsSourceDirs ["lib"]
      : buildDepends libraryDepends
      : commonOptions
    , []
    )
