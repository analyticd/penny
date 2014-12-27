module CartelCommon where

import qualified Cartel as C
import System.Environment

versionInts :: [Int]
versionInts = [0,33,0,0]

version :: C.Version
version = C.Version versionInts

-- Packages

base :: C.Package
base = C.closedOpen "base" [4,7,0,0] [4,8]

penny :: C.Package
penny = C.exactly "penny" versionInts

-- Options

ghcOptions :: [String]
ghcOptions = ["-Wall"]

repo :: C.Repository
repo = C.Repository
  { C.repoVcs = C.Git
  , C.repoKind = C.Head
  , C.repoLocation = "https://github.com/massysett/penny.git"
  , C.repoBranch = "master"
  , C.repoTag = ""
  , C.repoSubdir = ""
  }

testedWith :: [(C.Compiler, C.ConstrTree)]
testedWith =
  let ghc v = (C.GHC, C.Leaf (C.Constraint EQ (C.Version v)))
  in map ghc [[7,8,3]]

commonBuildInfo :: C.Field a => [a]
commonBuildInfo =
  [ C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

baseProperties :: C.Properties
baseProperties = C.empty
  { C.prName = "penny"
  , C.prVersion = version
  , C.prCabalVersion = (1,14)
  , C.prBuildType = C.Simple
  , C.prLicense = C.BSD3
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "2012 - 2014 Omari Norman"
  , C.prAuthor = "Omari Norman"
  , C.prMaintainer = "omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "http://www.github.com/massysett/penny"
  , C.prBugReports = "omari@smileystation.com"
  , C.prSynopsis = "Extensible double-entry accounting system"
  , C.prDescription =
    [ "Penny is a double-entry accounting system.  You keep your records"
    , "in a plain-text file, and Penny gives you useful reports in"
    , "your UNIX shell."
    , ""
    , "For more information, please see"
    , ""
    , "<http://www.github.com/massysett/penny>"
    ]
  , C.prCategory = "Console, Finance"
  , C.prTestedWith = testedWith
  }

libraryDepends :: [C.Package]
libraryDepends = [ base ]

commonOptions :: C.Field a => [a]
commonOptions = cond : commonBuildInfo
  where
    cond = C.cif (C.flag "debug")
      [ (C.ghcOptions ["-auto-all", "-caf-all", "-rtsopts"]) ]
      []

debug :: C.Flag
debug = C.Flag "debug"
  "Turns on debugging options" False True
