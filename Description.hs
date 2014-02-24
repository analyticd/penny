-- | Penny Cabal package description
--
-- Penny used to use an ordinary .cabal file, but it was growing
-- difficult to maintain due to large amounts of redundancy in the
-- file.  Writing it in Haskell brings the flexibility of Haskell to
-- bear on the problem.
module Description where

import Data.Tree
import Data.List
import qualified Distribution.Compiler as D
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.Version as D
import qualified Distribution.License as D
import qualified Distribution.ModuleName as D

versionA = 0
versionB = 32
versionC = 0
versionD = 1

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
  , D.customFieldsPD = []
  , D.buildDepends = []
  , D.specVersionRaw = Left $ D.Version [1, 14] []
  , D.buildType = Just D.Custom
  , D.library = Just library
  , D.executables = executables
  , D.testSuites = testSuites
  , D.benchmarks = []
  , D.dataFiles = []
  , D.dataDir = ""
  , D.extraSrcFiles = extraSrcFiles
  , D.extraTmpFiles = []
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

executables = undefined
library = undefined
testSuites = undefined
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
libModules =
  parent "Penny" $

    [ parent "Brenner" $
      map leaf [ "Clear", "Database", "Import", "Info",
                 "Merge", "OFX", "Print", "Types", "Util" ]

    , parent "Cabin"
      [ parent "Balance" $

        [ parent "Convert" $
          map leaf [ "Chunker", "ChunkerPct", "Options", "Parser" ]

        , parent "MultiCommodity" $
          map leaf [ "Chunker", "Parser" ]

        , leaf "Util"

        ] -- Balance

        ++ map leaf [ "Interface", "Meta", "Options", "Parsers" ]
        ++

        [ parent "Posts" $
          map leaf [ "Allocated", "BottomRows", "Fields", "Growers",
                     "Chunk", "Meta", "Parser", "Spacers", "Types" ]

        , leaf "Row"
        , parent "Scheme" [ leaf "Schemes" ]
        , leaf "TextFormat"
        ] -- Posts

      ] -- Cabin

    , parent "Copper" $
      map leaf [ "Interface", "Parsec", "Render", "Terminals" ]

    , parent "Denver" $
      map leaf [ "Diff", "Reprint", "Selloff", "Reconcile" ]

    , leaf "Liberty"

    , parent "Lincoln" $

          leaf "Balance"

        : parent "Bits" (map leaf [ "DateTime", "Open", "Price", "Qty" ])

        : map leaf [ "Builders", "Ents", "Equivalent", "HasText",
                      "Matchers", "Natural" ]

        ++ parent "Predicates" [leaf "Siblings"]

        : leaf "PriceDb"

        : parent "Queries" [leaf "Siblings"]

        : leaf "Serial"

        : []

    ]

    ++ map leaf [ "Shield", "Steel", "Wheat", "Zinc" ]

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
