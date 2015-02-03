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

uuParsing :: Package
uuParsing = closedOpen "uu-parsinglib" [2,8,1] [2,9]

parsec :: Package
parsec = closedOpen "parsec" [3,1,7] [3,2]

time :: Package
time = closedOpen "time" [1,4] [1,6]

transformers :: Package
transformers = closedOpen "transformers" [0,3] [0,5]

prednote :: Package
prednote = closedOpen "prednote" [0,28] [0,29]

semigroups :: Package
semigroups = closedOpen "semigroups" [0,16,1] [0,17]

quickcheck :: Package
quickcheck = closedOpen "QuickCheck" [2,7,6] [2,8]

derive :: Package
derive = closedOpen "derive" [2,5] [2,6]

tasty :: Package
tasty = closedOpen "tasty" [0,10,1] [0,11]

tastyQuickcheck :: Package
tastyQuickcheck = closedOpen "tasty-quickcheck" [0,8,3] [0,9]

tastyTh :: Package
tastyTh = closedOpen "tasty-th" [0,1,3] [0,2]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-Wall"]
  , haskell2010
  , hsSourceDirs ["lib"]
  ]

libraryDepends :: [Package]
libraryDepends =
  [ base
  , text
  , containers
  , uuParsing
  , time
  , transformers
  , prednote
  ]

testDepends :: [Package]
testDepends = [ quickcheck, derive, tasty, tastyQuickcheck,
                tastyTh ]

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
  testMods <- modules "tests"
  copper <- makeFlag "copper" $
    FlagOpts { flagDescription = "create copper-parse executable"
             , flagDefault = False
             , flagManual = True
             }
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ testSuite "penny-properties" $
          otherModules (testMods ++ libMods)
        : buildDepends (libraryDepends ++ testDepends)
        : hsSourceDirs ["tests"]
        : extensions ["TemplateHaskell"]
        : exitcodeFields "penny-properties.hs"
        ++ commonOptions
      , githubHead "massysett" "penny"
      , executable "copper-parse" $
          otherModules libMods
        : hsSourceDirs ["copper-parse"]
        : buildDepends libraryDepends
        : mainIs "copper-parse.hs"
        : commonOptions
      ]
    )
