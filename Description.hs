-- | Penny Cabal package description
--
-- Penny used to use an ordinary .cabal file, but it was growing
-- difficult to maintain due to large amounts of redundancy in the
-- file.  Writing it in Haskell brings the flexibility of Haskell to
-- bear on the problem.
module Description (genericDescription) where

import Data.Tree
import Data.List
import qualified Distribution.Compiler as D
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.Version as D
import qualified Distribution.License as D
import qualified Distribution.ModuleName as D
import qualified Language.Haskell.Extension as D

versionA = 0
versionB = 32
versionC = 0
versionD = 1

-- # Dependencies

-- ## Base
baseLowestVersion = [4, 6, 0, 0]
baseHighestVersion = [5]

base = D.Dependency (D.PackageName "base") ver
  where
    ver = D.intersectVersionRanges lower upper
    lower = D.thisVersion $ D.Version baseLowestVersion []
    upper = D.earlierVersion $ D.Version baseHighestVersion []

-- ## Penny itself
pennyDep = D.Dependency (D.PackageName "penny") $ D.thisVersion version

-- ## Omari dependencies

anonymous_sums          = dep "anonymous-sums"            [0,4,0,0]
matchers                = dep "matchers"                  [0,14,0,2]
multiarg                = dep "multiarg"                  [0,24,0,4]
ofx                     = dep "ofx"                       [0,4,0,2]
prednote                = dep "prednote"                  [0,18,0,4]
rainbow                 = dep "rainbow"                   [0,6,0,4]

-- ## Other dependencies

action_permutations     = dep "action-permutations"       [0,0,0,0]
bytestring              = dep "bytestring"                [0,10,0,2]
cereal                  = dep "cereal"                    [0,3,5,2]
containers              = dep "containers"                [0,4,2,1]
eitherPkg               = dep "either"                    [3,4,1]
old_locale              = dep "old-locale"                [1,0,0,5]
parsec                  = dep "parsec"                    [3,1,3]
pretty_show             = dep "pretty-show"               [1,5,0,0]
quickcheck              = dep "QuickCheck"                [2,5]
random                  = dep "random"                    [1,0,0,0]
random_shuffle          = dep "random-shuffle"            [0,0,4]
semigroups              = dep "semigroups"                [0,9,2]
split                   = dep "split"                     [0,2,2]
text                    = dep "text"                      [0,11,3,1]
time                    = dep "time"                      [1,4,0,1]
transformers            = dep "transformers"              [0,3,0,0]

name = D.PackageName "penny"
version = D.Version [versionA, versionB, versionC, versionD] []

packageDescription :: D.PackageDescription
packageDescription = D.emptyPackageDescription
  { D.package = D.PackageIdentifier name version
  , D.license = D.BSD3
  , D.licenseFile = "LICENSE"
  , D.copyright = "2012-2014 Omari Norman"
  , D.maintainer = "Omari Norman <omari@smileystation.com>"
  , D.author = "Omari Norman <omari@smileystation.com>"
  , D.stability = "Experimental"
  , D.testedWith = [ghc741, ghc762]
  , D.homepage = "http://www.github.com/massysett/penny"
  , D.pkgUrl = ""
  , D.bugReports = "omari@smileystation.com"
  , D.sourceRepos = [
      D.SourceRepo
      { D.repoKind = D.RepoHead
      , D.repoType = Just D.Git
      , D.repoLocation = Just "https://github.com/massysett/penny.git"
      , D.repoModule = Nothing
      , D.repoBranch = Just "master"
      , D.repoTag = Nothing
      , D.repoSubdir = Nothing
      }]
  , D.synopsis = synopsis
  , D.description = description
  , D.category = "Console, Finance"
  , D.specVersionRaw = Left $ D.Version [1, 14] []
  , D.buildType = Just D.Custom
  , D.library = Just library
  , D.executables = executables
  , D.testSuites = testSuites
  , D.extraSrcFiles = extraSrcFiles
  }

synopsis = "Extensible double-entry accounting system"

description = unlines
  [ "Penny is a double-entry accounting system.  You keep your records in a"
  , "plain-text file, and Penny gives you useful reports in your UNIX shell."
  , ""
  , "For more information, please see"
  , ""
  , "<http://www.github.com/massysett/penny>"
  ]

extraSrcFiles =
  [ "install-docs"
  , "README.md"
  , "doc/*.dot"
  , "doc/*.hs"
  , "doc/examples/*.pny"
  , "doc/man/*.1"
  , "doc/man/*.7"
  ]

libModules :: Modules
libModules = parent "Penny"
  (

    parent "Brenner" (
      map leaf [ "Clear", "Database", "Import", "Info",
                 "Merge", "OFX", "Print", "Types", "Util" ]
    ):

    parent "Cabin" (
      parent "Balance" (

        parent "Convert" (
          map leaf [ "Chunker", "ChunkerPct", "Options", "Parser" ]
        ):

        parent "MultiCommodity" (
          map leaf [ "Chunker", "Parser" ]
        ):

        leaf "Util":
        []
      ): -- Balance

      map leaf [ "Interface", "Meta", "Options", "Parsers" ] ++

      parent "Posts" (
        map leaf [ "Allocated", "BottomRows", "Fields", "Growers",
                   "Chunk", "Meta", "Parser", "Spacers", "Types" ]
      ):

      leaf "Row":
      
      parent "Scheme" [ leaf "Schemes" ]:
      leaf "TextFormat":
      []
    ) -- Cabin
    :

    parent "Copper" (
      map leaf [ "Interface", "Parsec", "Render", "Terminals" ]
    ):

    parent "Denver" (
      map leaf [ "Diff", "Reprint", "Selloff", "Reconcile" ]
    ):

    leaf "Liberty" :

    parent "Lincoln" (

      leaf "Balance":

      parent "Bits" (map leaf [ "DateTime", "Open", "Price", "Qty" ]):

      map leaf [ "Builders", "Ents", "Equivalent", "HasText",
                  "Matchers", "Natural" ] ++

      parent "Predicates" [leaf "Siblings"]:

      leaf "PriceDb":

      parent "Queries" [leaf "Siblings"]:

      leaf "Serial":

      []
    ):
    map leaf [ "Shield", "Steel", "Wheat", "Zinc" ]

  ) -- Penny

library = D.Library
  { D.exposedModules = modulesToList [] libModules
  , D.libExposed = True
  , D.libBuildInfo = libBuildInfo
  }

libBuildInfo = defaultBuildInfo
  { D.hsSourceDirs = ["lib"]
  }

libDepends =
  [ base
  , bytestring
  , containers
  , old_locale
  , parsec
  , split
  , text
  , time
  , transformers
  , anonymous_sums
  , matchers
  , multiarg
  , ofx
  , prednote
  , rainbow
  , action_permutations
  , cereal
  , eitherPkg
  , pretty_show
  , semigroups
  ]

-- # Executables

executables = undefined

-- ## Gibberish
exeGibberish = D.emptyExecutable
  { D.modulePath = "penny-gibberish.hs"
  , D.buildInfo = buildInfoGibberish
  }

depsGibberish =
  [ pennyDep
  , base
  , multiarg
  , quickcheck
  , random_shuffle
  , random
  , semigroups
  , text
  , time
  , transformers
  ]

buildInfoGibberish = defaultBuildInfo
  { D.hsSourceDirs = ["tests"]
  }

-- ## Simple executable
simpleExecutable
  :: FilePath
  -- ^ path to Main module
  -> D.Executable
simpleExecutable mp = D.emptyExecutable
  { D.modulePath = mp
  , D.buildInfo = defaultBuildInfo
    { D.hsSourceDirs = ["bin"]
    }
  }

exePenny = simpleExecutable "penny-main.hs"
exeSelloff = simpleExecutable "penny-selloff.hs"
exeDiff = simpleExecutable "penny-diff.hs"
exeReprint = simpleExecutable "penny-reprint.hs"
exeReconcile = simpleExecutable "penny-reconcile.hs"
exeDeps = [ pennyDep, base ]

-- # Test suite

modsTest =
  concatMap (modulesToList []) (
    parent "Copper" (
      parent "Gen" (
        leaf "Parsers":
        leaf "Terminals":
        []
      ):

      leaf "Parser":
      leaf "Render":
      []
    ):

    leaf "Lincoln":
    []
  )

buildInfoTest = defaultBuildInfo { D.hsSourceDirs = ["tests"] }

testDeps = 
  [ pennyDep
  , base
  , multiarg
  , anonymous_sums
  , quickcheck
  , random_shuffle
  , parsec
  , semigroups
  , text
  , time
  , transformers
  ]


testSuite = D.TestSuite
  { D.testName = ""
  , D.testInterface = D.TestSuiteExeV10
      (D.Version [1,0] []) "penny-test.hs"
  , D.testBuildInfo = buildInfoTest
  , D.testEnabled = True
  }

testSuites = [ testSuite ]

-- # Flags

exeFlag
  :: String
  -- ^ Executable name
  -> D.Flag
exeFlag n = D.MkFlag
  { D.flagName = D.FlagName $ "build-" ++ n
  , D.flagDescription = "build the " ++ n ++ " executable"
  , D.flagDefault = True
  , D.flagManual = True
  }

flagPenny = exeFlag "penny"
flagSelloff = exeFlag "penny-selloff"
flagDiff = exeFlag "penny-diff"
flagReprint = exeFlag "penny-reprint"
flagReconcile = exeFlag "penny-reconcile"

flagInCabal = D.MkFlag
  { D.flagName = D.FlagName "incabal"
  , D.flagDescription = "enables imports Cabal makes available"
  , D.flagDefault = True
  , D.flagManual = True
  }

flags = [ flagPenny, flagSelloff, flagDiff, flagReprint,
          flagReconcile, flagInCabal ]

-- # Conditional Library Tree
libraryTree :: D.CondTree D.ConfVar [D.Dependency] D.Library
libraryTree = D.CondNode
  { D.condTreeData = library
  , D.condTreeConstraints = libDepends
  , D.condTreeComponents = [subtree]
  }
  where
    subtree = (cond, tree, Nothing)
    cond = D.Var (D.Flag (D.FlagName "incabal"))
    tree = D.CondNode
      { D.condTreeData = D.emptyLibrary
        { D.exposedModules = []
        , D.libExposed = True
        , D.libBuildInfo = D.emptyBuildInfo
          { D.buildable = True
          , D.cppOptions = ["-Dincabal"]
          }
        }
      , D.condTreeConstraints = []
      , D.condTreeComponents = []
      }

-- # Executable trees

executableTree
  :: String
  -- ^ Executable name
  -> [D.Dependency]
  -- ^ Dependencies
  -> D.Executable
  -> (String, D.CondTree D.ConfVar [D.Dependency] D.Executable)
executableTree name deps exe = (name, tree)
  where
    tree = D.CondNode exe deps []

executableTrees =
  gibb:
  map f [ ("penny", exePenny)
        , ("penny-selloff", exeSelloff)
        , ("penny-diff", exeDiff)
        , ("penny-reprint", exeReprint)
        , ("penny-reconcile", exeReconcile)
        ]
  where
    gibb = (name, tree)
      where
        name = "penny-gibberish"
        tree = D.CondNode exeGibberish depsGibberish []
    f (n, ex) = (n, D.CondNode ex exeDeps [])

-- # Test suite trees

testSuiteTrees = [(name, tree)]
  where
    name = "penny-test"
    tree = D.CondNode testSuite testDeps []

-- # Generic description
genericDescription :: D.GenericPackageDescription
genericDescription = D.GenericPackageDescription
  { D.packageDescription = packageDescription
  , D.genPackageFlags = flags
  , D.condLibrary = Just libraryTree
  , D.condExecutables = executableTrees
  , D.condTestSuites = testSuiteTrees
  , D.condBenchmarks = []
  }

--
-- Helper functions
--

-- | Makes a GHC compiler version with the given A, B, and C version
-- components.
ghc :: Int -> Int -> Int -> (D.CompilerFlavor, D.VersionRange)
ghc a b c = (D.GHC, D.thisVersion $ D.Version [a, b, c] [])

ghc741 = ghc 7 4 1
ghc762 = ghc 7 6 2

-- | A tree of modules.  The Bool is True if the the given module
-- exists, or False if it does not.  The String is the name in the
-- hierarchy.
type Modules = Tree (Bool, String)

shell :: String -> [Modules] -> Modules
shell n = Node (False, n)

parent :: String -> [Modules] -> Modules
parent n = Node (True, n)

leaf :: String -> Modules
leaf n = Node (True, n) []

modulesToList :: [String] -> Modules -> [D.ModuleName]
modulesToList ns (Node (b, n) cs)
  | not b = restMods
  | otherwise = thisMod : restMods
  where
    restMods = concatMap (modulesToList (n:ns)) cs
    thisMod = D.fromString . concat . intersperse "." . reverse $ n:ns

dep :: String -> [Int] -> D.Dependency
dep n vs = D.Dependency (D.PackageName n) (D.orLaterVersion ver)
  where
    ver = D.Version vs []

defaultBuildInfo = D.emptyBuildInfo
  { D.buildable = True
  , D.otherModules = [ D.fromString "Paths_penny" ]
  , D.defaultLanguage = Just D.Haskell2010
  , D.options = [(D.GHC, ["-Wall"])]
  , D.ghcProfOptions = ["-auto-all", "-caf-all"]
  }

