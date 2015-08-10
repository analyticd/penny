-- | This file written for Cartel version 0.12.0.0

module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,0,0]

atleast :: NonEmptyString -> Version -> Package
atleast n v = package n (gtEq v)

-- Packages

base :: Package
base = closedOpen "base" [4,8,0,0] [4,9]

anonymousSums :: Package
anonymousSums = atleast "anonymous-sums" [0,4,0,0]

bytestring :: Package
bytestring = atleast "bytestring" [0,10,4]

text :: Package
text = atleast "text" [1,2]

containers :: Package
containers = atleast "containers" [0,5]

uuParsing :: Package
uuParsing = atleast "uu-parsinglib" [2,9,1]

-- | Currently unused
listlike :: Package
listlike = atleast "ListLike" [4,2]

-- | Currently unused
parsec :: Package
parsec = atleast "parsec" [3,1,7]

time :: Package
time = atleast "time" [1,4]

transformers :: Package
transformers = atleast "transformers" [0,3]

-- | Currently unused
prednote :: Package
prednote = atleast "prednote" [0,32]

quickcheck :: Package
quickcheck = atleast "QuickCheck" [2,7,6]

derive :: Package
derive = atleast "derive" [2,5]

tasty :: Package
tasty = atleast "tasty" [0,10,1]

tastyQuickcheck :: Package
tastyQuickcheck = atleast "tasty-quickcheck" [0,8,3]

tastyTh :: Package
tastyTh = atleast "tasty-th" [0,1,3]

bifunctors :: Package
bifunctors = atleast "bifunctors" [4,2]

rainbox :: Package
rainbox = atleast "rainbox" [0,18]

rainbow :: Package
rainbow = atleast "rainbow" [0,26]

semigroups :: Package
semigroups = atleast "semigroups" [0,16,1]

contravariant :: Package
contravariant = atleast "contravariant" [1,2]

pipes :: Package
pipes = atleast "pipes" [4,1,4]

pipesSafe :: Package
pipesSafe = atleast "pipes-safe" [2,2]

-- | Currently unused
process :: Package
process = atleast "process" [1,2,0,0]

async :: Package
async = atleast "async" [2,0]

turtle :: Package
turtle = atleast "turtle" [1,0,2]

mtl :: Package
mtl = atleast "mtl" [2,2,1]

pipesCliff :: Package
pipesCliff = atleast "pipes-cliff" [0,10]

logict :: Package
logict = atleast "logict" [0,6]

void :: Package
void = atleast "void" [0,7]

lens :: Package
lens = atleast "lens" [4,9]

operational :: Package
operational = atleast "operational" [0,2,3]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-W", "-fwarn-missing-signatures"]
  , haskell2010
  , hsSourceDirs ["lib"]
  , otherExtensions ["TemplateHaskell"]
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
  , bytestring
  , turtle
  , mtl
  , lens
  , async
  , pipes
  , pipesSafe
  , pipesCliff
  , process
  , anonymousSums
  , void
  , operational
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
