-- | Uses Cartel to generate the Cabal file.  Written for Cartel
-- version 0.2.0.0.

module Main (main) where

import qualified Cartel as A
import Prelude hiding (either)

version :: A.Version
version = A.Version [0,33,0,0]

-- Packages

penny :: A.Package
penny = A.Package "penny" (Just (A.Leaf (A.Constraint EQ version)))

base :: A.Package
base = A.closedOpen "base" [4,5,0,0] [4,8]

bytestring :: A.Package
bytestring = A.closedOpen "bytestring" [0,9,2,1] [0,11]

containers :: A.Package
containers = A.closedOpen "containers" [0,4,2,1] [0,6]

old_locale :: A.Package
old_locale = A.closedOpen "old-locale" [1,0,0,4] [1,1]

parsec :: A.Package
parsec = A.closedOpen "parsec" [3,1,3] [3,2]

split :: A.Package
split = A.closedOpen "split" [0,2,2] [0,3]

text :: A.Package
text = A.closedOpen "text" [0,11,3,1] [1,2]

time :: A.Package
time = A.closedOpen "time" [1,4] [1,5]

transformers :: A.Package
transformers = A.closedOpen "transformers" [0,3,0,0] [0,4]

action_permutations :: A.Package
action_permutations = A.closedOpen "action-permutations"
  [0,0,0,0] [0,0,0,2]

cereal :: A.Package
cereal = A.closedOpen "cereal" [0,3,5,2] [0,5]

contravariant :: A.Package
contravariant = A.closedOpen "contravariant" [0,2,0,1] [0,5]

either :: A.Package
either = A.closedOpen "either" [3,4,1] [4,2]

semigroups :: A.Package
semigroups = A.closedOpen "semigroups" [0,9,2] [0,14]

quickcheck :: A.Package
quickcheck = A.closedOpen "QuickCheck" [2,6] [2,7]

tasty :: A.Package
tasty = A.closedOpen "tasty" [0,8] [0,9]

tasty_quickcheck :: A.Package
tasty_quickcheck = A.closedOpen "tasty-quickcheck" [0,8] [0,9]

random_shuffle :: A.Package
random_shuffle = A.exactly "random-shuffle" [0,0,4]


-- Omari packages

anonymous_sums :: A.Package
anonymous_sums = A.nextBreaking "anonymous-sums" [0,4,0,0]

matchers :: A.Package
matchers = A.nextBreaking "matchers" [0,18,0,0]

multiarg :: A.Package
multiarg = A.closedOpen "multiarg" [0,24,0,4] [0,27,0,0]

ofx :: A.Package
ofx = A.nextBreaking "ofx" [0,4,0,2]

prednote :: A.Package
prednote = A.nextBreaking "prednote" [0,22,0,2]

rainbow :: A.Package
rainbow = A.nextBreaking "rainbow" [0,14,0,0]

rainbox :: A.Package
rainbox = A.nextBreaking "rainbox" [0,4,0,2]

-- End Packages

ghcOptions :: [String]
ghcOptions = ["-Wall"]

flags :: [A.Flag]
flags =
  [ A.Flag "debug" "turns on debugging options" False True ]

libraryDepends :: [A.Package]
libraryDepends =
  [ base
  , bytestring
  , containers
  , old_locale
  , parsec
  , split
  , text
  , time
  , transformers
  , action_permutations
  , cereal
  , contravariant
  , either
  , semigroups
  , anonymous_sums
  , matchers
  , multiarg
  , ofx
  , prednote
  , rainbow
  , rainbox
  ]

testedWith :: [(A.Compiler, A.ConstrTree)]
testedWith =
  let ghc v = (A.GHC, A.Leaf (A.Constraint EQ (A.Version v)))
  in map ghc [[7,4,1], [7,6,3], [7,8,2]]

repo :: A.Repository
repo = A.Repository
  { A.repoVcs = A.Git
  , A.repoKind = A.Head
  , A.repoLocation = "https://github.com/massysett/penny.git"
  , A.repoBranch = "master"
  , A.repoTag = ""
  , A.repoSubdir = ""
  }

extraSourceFiles :: [String]
extraSourceFiles =
  [ "install-docs"
  , "README.md"
  , "doc/*.dot"
  , "doc/*.hs"
  , "doc/examples/*.pny"
  , "doc/man/*.1"
  , "doc/man/*.7"
  , "versions.m4"
  , "Makefile"
  , "current-versions.txt"
  , "minimum-versions.txt"
  , "changelog"
  , "genCabal.hs"
  ]

properties :: A.Properties
properties = A.properties
  { A.prName = "penny"
  , A.prVersion = version
  , A.prCabalVersion = (1,14)
  , A.prBuildType = A.Simple
  , A.prLicense = A.BSD3
  , A.prLicenseFile = "LICENSE"
  , A.prCopyright = "2012 - 2014 Omari Norman"
  , A.prAuthor = "Omari Norman"
  , A.prMaintainer = "omari@smileystation.com"
  , A.prStability = "Experimental"
  , A.prHomepage = "http://www.github.com/massysett/penny"
  , A.prBugReports = "omari@smileystation.com"
  , A.prSynopsis = "Extensible double-entry accounting system"
  , A.prDescription =
    [ "Penny is a double-entry accounting system.  You keep your records"
    , "in a plain-text file, and Penny gives you useful reports in"
    , "your UNIX shell."
    , ""
    , "For more information, please see"
    , ""
    , "<http://www.github.com/massysett/penny>"
    ]
  , A.prCategory = "Console, Finance"
  , A.prTestedWith = testedWith
  , A.prExtraSourceFiles = extraSourceFiles
  }

commonBuildInfo :: [A.BuildInfoField]
commonBuildInfo =
  [ A.GHCOptions ghcOptions
  , A.DefaultLanguage A.Haskell2010
  ]

commonOptions :: A.Field a => [a]
commonOptions = cond : map A.buildInfo commonBuildInfo
  where
    cond = A.conditional $ A.CondBlock (A.CLeaf . A.CFlag $ "debug")
      [ A.buildInfo (A.GHCOptions ["-auto-all", "-caf-all"]) ]
      []

library
  :: [String]
  -- ^ Library modules
  -> A.Library
library ms = A.Library $
  [ A.LibExposedModules ms
  , A.buildInfo $ A.OtherModules ["Paths_penny"]
  , A.LibExposed True
  , A.buildInfo (A.HsSourceDirs ["lib"])
  , A.buildInfo $ A.BuildDepends libraryDepends
  ]

  ++ commonOptions

pennyTestDepends :: [A.Package]
pennyTestDepends =
  [ penny
  , base
  , anonymous_sums
  , quickcheck
  , tasty
  , tasty_quickcheck
  , random_shuffle
  , parsec
  , semigroups
  , text
  , time
  , transformers
  ]

pennyTest
  :: [String]
  -- ^ Test modules
  -> A.TestSuite
pennyTest ms = A.TestSuite "penny-test" $
  [ A.TestType $ A.ExitcodeStdio "penny-test.hs"
  , A.buildInfo $ A.OtherModules ms
  , A.buildInfo $ A.BuildDepends pennyTestDepends
  , A.buildInfo $ A.HsSourceDirs ["tests"]
  ]

  ++ commonOptions

cabal
  :: [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> A.Cabal
cabal libMods testMods = A.cabal
  { A.cProperties = properties
  , A.cRepositories = [repo]
  , A.cFlags = flags
  , A.cLibrary = Just (library libMods)
  , A.cTestSuites = [pennyTest testMods]
  }

main :: IO ()
main = do
  libMods <- A.modules "lib"
  testMods <- A.modules "tests"
  let c = cabal libMods testMods
  A.render "genCabal.hs" c
