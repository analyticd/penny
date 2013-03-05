module Penny.Wheat where

import Control.Arrow (second)
import Control.Monad (join)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import qualified Penny.Steel.Prednote as N
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Lincoln.Predicates as P
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Penny.Lincoln.Builders as Bd
import Data.Text (Text, pack)
import qualified Data.Time as Time
import qualified Text.Matchers as M
import qualified Text.Parsec as Parsec
import qualified System.Console.MultiArg as MA
import qualified System.Console.Terminfo as TI
import qualified System.Exit as Exit
import System.Locale (defaultTimeLocale)
import qualified System.IO as IO
import qualified Penny.Shield as S

import Text.Matchers
import Penny.Steel.Prednote hiding (Pdct)
import qualified Penny.Steel.Predtree as Pt
import qualified Penny.Steel.Chunk as C

--
-- Types
--

type Pass = Bool
type Name = Text
data Tree a = Tree Name (Payload a)

data Payload a
  = Group [Tree a]
  | Test (TestFunc a)

type TestFunc a
  = Pt.IndentAmt
  -> Pt.ShowDiscards
  -> Pt.Level
  -> [a]
  -> (Pass, [C.Chunk])


group :: Name -> [Tree a] -> Tree a
group n ts = Tree n (Group ts)

test :: Name -> TestFunc a -> Tree a
test n t = Tree n (Test t)



{-
------------------------------------------------------------
-- Series
------------------------------------------------------------

-- | True if the list is at least n elements long. Less strict than
-- 'length'.
atLeast :: Int -> [a] -> Bool
atLeast x = go 0
  where
    go i [] = i >= x
    go i (_:as) =
      if i >= x
      then True
      else let i' = i + 1 in i' `seq` go i' as

-- | Passes if at least n subjects are True.
seriesAtLeastN
  :: Name
  -> Int
  -> Pdct a
  -> Test a
seriesAtLeastN name i (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs = (atl, name, pairs)
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        atl = atLeast i
              . filter id
              . map ((\(p, _, _) -> p) . T.rootLabel)
              $ rslts

-- | Every subject is run through the test. Each subject must return
-- True; otherwise the test fails.
eachSubjectMustBeTrue
  :: Name
  -> Pdct a
  -> Test a
eachSubjectMustBeTrue n (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs =
      let mkOut = (zip pfs)
                  &&& (all ((\(p, _, _) -> p) . T.rootLabel))
          toTup (pairs, passed) = (passed, n, pairs)
          mkRslts = (pure t <*>)
      in toTup . mkOut . mkRslts $ pfs

-- | Every subject is run through the test. Subjects that return True
-- are then fed to the given function. The result of the function is
-- the result of the test.
processTrueSubjects
  :: Name
  -> ([a] -> Bool)
  -> Pdct a
  -> Test a
processTrueSubjects name fp (Pdct t) = Test $ T.Node (Left fn) []
  where
    fn pfs = (r, name, pairs)
      where
        pairs = zip pfs rslts
        rslts = pure t <*> pfs
        r = fp . map fst
            . filter ((\(p, _, _) -> p) . T.rootLabel . snd) $ pairs

group :: Name -> [Test a] -> Test a
group n = Test . T.Node (Right n) . map unTest

--
-- Verbosity
--

data Verbosity
  = Silent
  | Status
  | Interesting
  | All
  deriving (Eq, Ord, Show)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity
type SpaceCount = Int


------------------------------------------------------------
-- Other conveniences
------------------------------------------------------------


-- | A non-terminating list of starting with the first day of the
-- first month following the given day, followed by successive first
-- days of the month.
futureFirstsOfTheMonth :: Time.Day -> [Time.Day]
futureFirstsOfTheMonth d = iterate (Time.addGregorianMonthsClip 1) d1
  where
    d1 = Time.fromGregorian yr mo 1
    (yr, mo, _) = Time.toGregorian $ Time.addGregorianMonthsClip 1 d

------------------------------------------------------------
-- CLI
------------------------------------------------------------

type ProgName = String
type BriefDesc = String
type ColorToFile = Bool
type BaseTime = L.DateTime
type MoreHelp = [String]

help
  :: WheatConf
  -> ProgName
  -> String
help c pn = unlines $
  [ "usage: " ++ pn ++ "[options] ARGS"
  , ""
  , briefDescription c
  , "Options:"
  , ""
  , "--color-to-file no|yes"
  , "  If yes, use colors even when standard output is"
  , "  not a terminal. (default: " ++ dCtf ++ ")"
  , ""
  , "--pass-verbosity, -p VERBOSITY"
  , "--fail-verbosity, -f VERBOSITY"
  , "  Use the given level of verbosity for passing or for"
  , "  failing tests (each may be set independently.) Choices:"
  , "    silent - show nothing at all"
  , "    status - show only that the test passed or failed"
  , "    interesting - show that the test passed or failed, and"
  , "      the interesting results of the underlying predicates"
  , "    all - show that the test passed or failed, and all"
  , "      results from the underlying predicates"
  , "    (default pass verbosity: " ++ descV (passVerbosity c) ++ ")"
  , "    (default fail verbosity: " ++ descV (failVerbosity c) ++ ")"
  , ""
  , "--indentation, -i SPACES"
  , "  indent each level by this many spaces"
  , "  (default: " ++ dSc ++ ")"
  , ""
  , "--base-date, -d DATE"
  , "  use this date as basis for checks"
  , "  (currently: " ++ showDateTime (baseTime c) ++ ")"
  , ""
  , "--help, -h - show help and exit"
  , ""
  ] ++ moreHelp c
  where
    dCtf = if colorToFile c then "yes" else "no"
    descV v = case v of
      N.Silent -> "silent"
      N.Status -> "status"
      N.Interesting -> "interesting"
      N.All -> "all"
    dSc = show (spaceCount c)

showDateTime :: L.DateTime -> String
showDateTime (L.DateTime d h m s tz) =
  ds ++ " " ++ hmss ++ " " ++ showOffset
  where
    ds = show d
    hmss = hs ++ ":" ++ ms ++ ":" ++ ss
    hs = pad0 . show . L.unHours $ h
    ms = pad0 . show . L.unMinutes $ m
    ss = pad0 . show . L.unSeconds $ s
    pad0 str = if length str < 2 then '0':str else str
    showOffset =
      let (zoneHr, zoneMin) = abs (L.offsetToMins tz) `divMod` 60
          sign = if L.offsetToMins tz < 0 then "-" else "+"
      in sign ++ pad0 (show zoneHr) ++ pad0 (show zoneMin)

data Arg
  = APassV Verbosity
  | AFailV Verbosity
  | AColorToFile ColorToFile
  | AIndentation SpaceCount
  | ABaseTime L.DateTime
  | APosArg String
  deriving Eq

lsVerbosity :: [(String, N.Verbosity)]
lsVerbosity = [ ("silent", N.Silent)
              , ("status", N.Status)
              , ("interesting", N.Interesting)
              , ("all", N.All)
              ]

optPassVerbosity :: MA.OptSpec Arg
optPassVerbosity = MA.OptSpec ["pass-verbosity"] "p" (MA.ChoiceArg ls)
  where
    ls = fmap (second APassV) lsVerbosity

optFailVerbosity :: MA.OptSpec Arg
optFailVerbosity = MA.OptSpec ["fail-verbosity"] "f" (MA.ChoiceArg ls)
  where
    ls = fmap (second AFailV) lsVerbosity

optColorToFile :: MA.OptSpec Arg
optColorToFile = MA.OptSpec ["color-to-file"] "" (MA.ChoiceArg ls)
  where
    ls = fmap (second AColorToFile) [ ("no", False), ("yes", True) ]

type ExS = Ex.Exceptional String
optIndentation :: MA.OptSpec (ExS Arg)
optIndentation = MA.OptSpec ["indentation"] "i" (MA.OneArg f)
  where
    f s =
      let err = Ex.throw $ "improper indentation argument: " ++ s
      in case reads s of
          (i, ""):[] ->
            if i >= 0 then Ex.Success (AIndentation i) else err
          _ -> err

optBaseTime :: MA.OptSpec (ExS Arg)
optBaseTime = MA.OptSpec ["base-date"] "b" (MA.OneArg f)
  where
    f s = case Parsec.parse CP.dateTime  "" (X.pack s) of
      Left e -> Ex.throw $ "could not parse date: " ++ show e
      Right g -> return . ABaseTime $ g

data ParsedOpts = ParsedOpts
  { pPassVerbosity :: PassVerbosity
  , pFailVerbosity :: FailVerbosity
  , pSpaceCount :: SpaceCount
  , pColorToFile :: ColorToFile
  , pBaseTime :: BaseTime
  , pPosArgs :: [String]
  }

allOpts :: [MA.OptSpec (ExS Arg)]
allOpts =
  [ fmap return optPassVerbosity
  , fmap return optFailVerbosity
  , fmap return optColorToFile
  , optIndentation
  , optBaseTime
  ]

parseArgs
  :: WheatConf
  -> [Arg]
  -> ParsedOpts
parseArgs c as = ParsedOpts
  { pPassVerbosity = getPassVerbosity (passVerbosity c) as
  , pFailVerbosity = getFailVerbosity (failVerbosity c) as
  , pSpaceCount = getSpaceCount (spaceCount c) as
  , pColorToFile = getColorToFile (colorToFile c) as
  , pBaseTime = getBaseTime (baseTime c) as
  , pPosArgs = getPosArg as
  }

getPassVerbosity :: N.PassVerbosity -> [Arg] -> N.PassVerbosity
getPassVerbosity v as = case mapMaybe f as of
  [] -> v
  xs -> last xs
  where f a = case a of { APassV vb -> Just vb; _ -> Nothing }

getFailVerbosity :: N.FailVerbosity -> [Arg] -> N.FailVerbosity
getFailVerbosity v as = case mapMaybe f as of
  [] -> v
  xs -> last xs
  where f a = case a of { AFailV vb -> Just vb; _ -> Nothing }

getSpaceCount :: N.SpaceCount -> [Arg] -> N.SpaceCount
getSpaceCount sc as = case mapMaybe f as of
  [] -> sc
  xs -> last xs
  where f a = case a of { AIndentation i -> Just i; _ -> Nothing }

getColorToFile :: ColorToFile -> [Arg] -> ColorToFile
getColorToFile ctf as = case mapMaybe f as of
  [] -> ctf
  xs -> last xs
  where f a = case a of { AColorToFile i -> Just i; _ -> Nothing }

getPosArg :: [Arg] -> [String]
getPosArg = mapMaybe f
  where f a = case a of { APosArg s -> Just s; _ -> Nothing }

getBaseTime :: BaseTime -> [Arg] -> BaseTime
getBaseTime bd as = case mapMaybe f as of
  [] -> bd
  xs -> last xs
  where f a = case a of { ABaseTime x -> Just x; _ -> Nothing }

data WheatConf = WheatConf
  { briefDescription :: String
  , moreHelp :: [String]
  , passVerbosity :: N.PassVerbosity
  , failVerbosity :: N.FailVerbosity
  , spaceCount :: N.SpaceCount
  , colorToFile :: ColorToFile
  , tests :: BaseTime -> [N.Test L.PostFam]
  , baseTime :: BaseTime
  }

getArgsAndParse :: WheatConf -> IO ParsedOpts
getArgsAndParse c = do
  pn <- MA.getProgName
  as <- MA.simpleWithHelp (help c) MA.Intersperse allOpts APosArg
  args <- case sequence as of
    Ex.Exceptional s -> do
      IO.hPutStrLn IO.stderr $ pn ++ ": error: " ++ s
      Exit.exitFailure
    Ex.Success g -> return g
  return $ parseArgs c args

wheatMain :: (S.Runtime -> WheatConf) -> IO ()
wheatMain getConf = do
  rt <- S.runtime
  pn <- MA.getProgName
  let c = getConf rt
  po <- getArgsAndParse c
  let getTerm =
        if ctf || (S.output rt == S.IsTTY)
        then TI.setupTermFromEnv
        else TI.setupTerm "dumb"
  ti <- getTerm

{-
  let runTests is = map (N.runTest pv fv is 0) (tests c bt)
  good <- fmap and
          . join
          . fmap (mapM (N.showResults ti sc display))
          . fmap runTests
          $ getItems pn posargs
  if good
    then Exit.exitSuccess
    else Exit.exitFailure
-}
-- | Displays a PostFam in a one line format.
--
-- Format:
--
-- File LineNo Date Payee Acct DrCr Cmdty Qty
display :: L.PostFam -> Text
display p = X.pack $ concat (intersperse " " ls)
  where
    ls = [file, lineNo, dt, pye, acct, dc, cmdty, qt]
    file = maybe (labelNo "filename") (X.unpack . L.unFilename)
           (Q.filename p)
    lineNo = maybe (labelNo "line number")
             (show . L.unPostingLine) (Q.postingLine p)
    dateFormat = "%Y-%m-%d %T %z"
    dt = Time.formatTime defaultTimeLocale dateFormat
         . Time.utctDay
         . L.toUTC
         . Q.dateTime
         $ p
    pye = maybe (labelNo "payee")
            (X.unpack . L.text) (Q.payee p)
    acct = X.unpack . X.intercalate (X.singleton ':')
           . map L.unSubAccount . L.unAccount . Q.account $ p
    dc = case Q.drCr p of
      L.Debit -> "Dr"
      L.Credit -> "Cr"
    cmdty = X.unpack . L.unCommodity . Q.commodity $ p
    qt = show . Q.qty $ p

labelNo :: String -> String
labelNo s = "(no " ++ s ++ ")"

getItems :: ProgName -> [String] -> IO [L.PostFam]
getItems pn ss = Cop.openStdin ss >>= f
  where
    f res = case res of
      Ex.Exception e -> do
        IO.hPutStrLn IO.stderr $ pn
          ++ ": error: could not parse ledgers: "
          ++ (X.unpack . Cop.unErrorMsg $ e)
        Exit.exitFailure
      Ex.Success g ->
        let toTxn i = case i of { Cop.Transaction x -> Just x; _ -> Nothing }
        in return . concatMap L.postFam
           . mapMaybe toTxn . Cop.unLedger $ g

-}
