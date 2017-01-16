module Main where

import Cartel

pennyVer :: [Word]
pennyVer = [0,33,20170116,0]

warnings :: [String]
warnings =
  [ "-W"
  ]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions warnings
  , haskell2010
  , hsSourceDirs ["lib"]
  , otherExtensions ["TemplateHaskell"]
  ]

base :: Package
base = closedOpen "base" [4,9,0,0] [5]

anonymousSums :: Package
anonymousSums = atLeast "anonymous-sums" [0,4,0,0]

bytestring :: Package
bytestring = atLeast "bytestring" [0,10,4]

text :: Package
text = atLeast "text" [1,2]

containers :: Package
containers = atLeast "containers" [0,5]

time :: Package
time = atLeast "time" [1,4]

transformers :: Package
transformers = atLeast "transformers" [0,3]

quickcheck :: Package
quickcheck = atLeast "QuickCheck" [2,7,6]

tasty :: Package
tasty = atLeast "tasty" [0,11]

tastyHunit :: Package
tastyHunit = atLeast "tasty-hunit" [0,9,2]

tastyQuickcheck :: Package
tastyQuickcheck = atLeast "tasty-quickcheck" [0,8,3]

tastyTh :: Package
tastyTh = atLeast "tasty-th" [0,1,3]

bifunctors :: Package
bifunctors = atLeast "bifunctors" [4,2]

rainbox :: Package
rainbox = atLeast "rainbox" [0,18]

rainbow :: Package
rainbow = atLeast "rainbow" [0,26]

turtle :: Package
turtle = atLeast "turtle" [1,3,0]

mtl :: Package
mtl = atLeast "mtl" [2,2,1]

lens :: Package
lens = atLeast "lens" [4,9]

operational :: Package
operational = atLeast "operational" [0,2,3]

pretty :: Package
pretty = atLeast "pretty" [1,1,2]

derive :: Package
derive = atLeast "derive" [2,5,22]

earley :: Package
earley = atLeast "Earley" [0,10,1,0]

pinchot :: Package
pinchot = atLeast "pinchot" [0,22,0,0]

accuerr :: Package
accuerr = atLeast "accuerr" [0,2,0,0]

ofx :: Package
ofx = atLeast "ofx" [0,4,2,0]

timelens :: Package
timelens = atLeast "timelens" [0,2]

prettyShow :: Package
prettyShow = atLeast "pretty-show" [1,6]

templateHaskell :: Package
templateHaskell = atLeast "template-haskell" [2,10]

nonEmptySequence :: Package
nonEmptySequence = atLeast "non-empty-sequence" [0,2]

formatting :: Package
formatting = atLeast "formatting" [6,2,4]

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
  , bytestring
  , turtle
  , mtl
  , lens
  , anonymousSums
  , pretty
  , pinchot
  , earley
  , accuerr
  , ofx
  , timelens
  , prettyShow
  , templateHaskell
  , nonEmptySequence
  , formatting
  , pinchot
  , earley
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
