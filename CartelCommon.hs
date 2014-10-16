module CartelCommon where

import qualified Cartel as A

versionInts :: [Int]
versionInts = [0,33,0,0]

version :: A.Version
version = A.Version versionInts

-- Packages

base :: A.Package
base = A.closedOpen "base" [4,5,0,0] [4,8]

bytestring :: A.Package
bytestring = A.closedOpen "bytestring" [0,9,2,1] [0,11]

containers :: A.Package
containers = A.closedOpen "containers" [0,4,2,1] [0,6]

bifunctors :: A.Package
bifunctors = A.closedOpen "bifunctors" [0,1] [4,2]

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
transformers = A.closedOpen "transformers" [0,3,0,0] [0,5]

action_permutations :: A.Package
action_permutations = A.closedOpen "action-permutations"
  [0,0,0,0] [0,0,0,2]

cereal :: A.Package
cereal = A.closedOpen "cereal" [0,3,5,2] [0,5]

contravariant :: A.Package
contravariant = A.closedOpen "contravariant" [0,2,0,1] [0,7]

either :: A.Package
either = A.closedOpen "either" [3,4,1] [4,4]

semigroups :: A.Package
semigroups = A.closedOpen "semigroups" [0,9,2] [0,16]

quickcheck :: A.Package
quickcheck = A.closedOpen "QuickCheck" [2,7] [2,8]

random_shuffle :: A.Package
random_shuffle = A.exactly "random-shuffle" [0,0,4]

random :: A.Package
random = A.closedOpen "random" [1,0,0,0] [1,1]

-- Omari packages

anonymous_sums :: A.Package
anonymous_sums = A.nextBreaking "anonymous-sums" [0,4,0,0]

anonymous_sums_tests :: A.Package
anonymous_sums_tests = A.nextBreaking "anonymous-sums-tests" [0,4,0,0]

matchers :: A.Package
matchers = A.nextBreaking "matchers" [0,20,0,0]

multiarg :: A.Package
multiarg = A.closedOpen "multiarg" [0,24,0,4] [0,27,0,0]

ofx :: A.Package
ofx = A.nextBreaking "ofx" [0,4,0,2]

prednote :: A.Package
prednote = A.nextBreaking "prednote" [0,24,2,0]

rainbow :: A.Package
rainbow = A.nextBreaking "rainbow" [0,14,0,0]

rainbox :: A.Package
rainbox = A.nextBreaking "rainbox" [0,4,0,2]

quickpull :: A.Package
quickpull = A.nextBreaking "quickpull" [0,2,0,0]

penny :: A.Package
penny = A.exactly "penny" versionInts

dekaTests :: A.Package
dekaTests = A.nextBreaking "deka-tests" [0,6,0,0]

barecheck :: A.Package
barecheck = A.nextBreaking "barecheck" [0,2,0,0]

-- End Packages

ghcOptions :: [String]
ghcOptions = ["-Wall"]

manualFlag
  :: String
  -- ^ Name
  -> String
  -- ^ Description
  -> Bool
  -- ^ Default
  -> A.Flag
manualFlag n ds df = A.empty
  { A.flName = n
  , A.flDescription = ds
  , A.flDefault = df
  , A.flManual = True
  }

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

baseProperties :: A.Properties
baseProperties = A.empty
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
  }

commonBuildInfo :: A.Field a => [a]
commonBuildInfo =
  [ A.ghcOptions ghcOptions
  , A.defaultLanguage A.Haskell2010
  ]

