module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,0,0]

atleast :: NonEmptyString -> Version -> Package
atleast n v = package n (gtEq v)

-- Packages

base :: Package
base = closedOpen "base" [4,9,0,0] [5]

anonymousSums :: Package
anonymousSums = atleast "anonymous-sums" [0,4,0,0]

bytestring :: Package
bytestring = atleast "bytestring" [0,10,4]

text :: Package
text = atleast "text" [1,2]

containers :: Package
containers = atleast "containers" [0,5]

time :: Package
time = atleast "time" [1,4]

transformers :: Package
transformers = atleast "transformers" [0,3]

quickcheck :: Package
quickcheck = atleast "QuickCheck" [2,7,6]

tasty :: Package
tasty = atleast "tasty" [0,11]

tastyHunit :: Package
tastyHunit = atleast "tasty-hunit" [0,9,2]

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

pretty :: Package
pretty = atleast "pretty" [1,1,2]

derive :: Package
derive = atleast "derive" [2,5,22]

managed :: Package
managed = atleast "managed" [1,0]

hspec :: Package
hspec = atleast "hspec" [2,2]

monoidSubclasses :: Package
monoidSubclasses = atleast "monoid-subclasses" [0,4,1]

pinchot :: Package
pinchot = atleast "pinchot" [0,18,0,0]

earley :: Package
earley = atleast "Earley" [0,10,1,0]

accuerr :: Package
accuerr = atleast "accuerr" [0,2,0,0]

ofx :: Package
ofx = atleast "ofx" [0,4,0,4]

parsec :: Package
parsec = atleast "parsec" [3,1,9]

pennyCopper :: Package
pennyCopper = exactly "penny-copper" pennyVer

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
  , pipesCliff
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
  , pennyCopper
  ]

testDepends :: [Package]
testDepends = [ quickcheck, tasty, tastyQuickcheck, tastyTh, derive ]

props :: Properties
props = blank
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
  copper <- makeFlag "copper"
    FlagOpts { flagDescription = "create copper-parse executable"
             , flagDefault = False
             , flagManual = True
             }
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny"
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
      ]
    )
