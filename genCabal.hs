-- | This file written for Cartel version 0.12.0.0

module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,0,0]

-- Packages

base :: Package
base = closedOpen "base" [4,7,0,0] [4,8]

anonymousSums :: Package
anonymousSums = closedOpen "anonymous-sums" [0,4,0,0] [0,5]

bytestring :: Package
bytestring = closedOpen "bytestring" [0,10,4] [0,11]

text :: Package
text = closedOpen "text" [1,2] [1,3]

containers :: Package
containers = closedOpen "containers" [0,5] [0,6]

uuParsing :: Package
uuParsing = closedOpen "uu-parsinglib" [2,8,1] [2,9]

-- | Currently unused
parsec :: Package
parsec = closedOpen "parsec" [3,1,7] [3,2]

time :: Package
time = closedOpen "time" [1,4] [1,6]

transformers :: Package
transformers = closedOpen "transformers" [0,3] [0,5]

-- | Currently unused
prednote :: Package
prednote = closedOpen "prednote" [0,32] [0,33]

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

bifunctors :: Package
bifunctors = closedOpen "bifunctors" [4,2] [4,3]

rainbox :: Package
rainbox = closedOpen "rainbox" [0,10,0,0] [0,11]

rainbow :: Package
rainbow = closedOpen "rainbow" [0,22] [0,23]

semigroups :: Package
semigroups = closedOpen "semigroups" [0,16,1] [0,17]

contravariant :: Package
contravariant = closedOpen "contravariant" [1,2] [1,3]

pipes :: Package
pipes = closedOpen "pipes" [4,1,4] [4,2]

-- | Currently unused
pipesSafe :: Package
pipesSafe = closedOpen "pipes-safe" [2,2] [2,3]

-- | Currently unused
process :: Package
process = closedOpen "process" [1,2,0,0] [1,3]

-- | Currently unused
async :: Package
async = closedOpen "async" [2,0] [2,1]

turtle :: Package
turtle = closedOpen "turtle" [1,0,2] [1,1]

mtl :: Package
mtl = closedOpen "mtl" [2,2,1] [2,3]

pipesCliff :: Package
pipesCliff = closedOpen "pipes-cliff" [0,10] [0,11]

-- | Currently unused
logict :: Package
logict = closedOpen "logict" [0,6] [0,7]

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
  , bifunctors
  , rainbow
  , rainbox
  , semigroups
  , contravariant
  , pipes
  , bytestring
  , turtle
  , mtl
  , pipesCliff
  , anonymousSums
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
        : defaultExtensions ["TemplateHaskell"]
        : ghcOptions ["-threaded"]
        : exitcodeFields "penny-properties.hs"
        ++ commonOptions
      , githubHead "massysett" "penny"
      , executable "copper-parse" $
          [ mainIs "copper-parse.hs"
          , condBlock (flag copper)
              ( otherModules libMods
              , [ hsSourceDirs ["copper-parse"]
                , buildDepends libraryDepends
                ] ++ commonOptions
              )
              [ buildable False ]
          ]
      ]
    )
