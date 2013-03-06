{-# LANGUAGE OverloadedStrings #-}
module Penny.Wheat where

import Control.Arrow (second)
import Control.Monad (join)
import Control.Monad.Loops (unfoldrM)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Control.Monad.Trans.State as St
import Data.List (intersperse, unfoldr)
import Data.Maybe (mapMaybe, catMaybes, isJust)
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

import qualified Penny.Steel.Predtree as Pt
import qualified Penny.Steel.Chunk as C
import qualified Penny.Steel.Chunk.Switch as Sw

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
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> Pt.Level
  -> (Pass, [C.Chunk])


group :: Name -> [Tree a] -> Tree a
group n ts = Tree n (Group ts)

test :: Name -> TestFunc a -> Tree a
test n t = Tree n (Test t)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity

data Verbosity
  = Silent
  -- ^ Show nothing at all

  | PassFail
  -- ^ Show only whether the test passed or failed

  | FalseSubjects
  -- ^ Show subjects that are False

  | TrueSubjects
  -- ^ Show subjects that are True. (This is cumulative, so False
  -- subjects are shown too.)

  | DiscardedSubjects

  | DiscardedPredicates
  -- ^ Show discarded results
  deriving (Eq, Ord, Show)

--
-- Helper functions
--

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


-- | Takes as many elements from a list as necessary until the given
-- number of elements that have Just True as the second element of the
-- pair are found.  Will stop processing if the number of elements is
-- found, but does not discard elements solely because they do not
-- satisfy the predicate. Returns the number of elements found along
-- with the list.
takeCount
  :: Int
  -- ^ Find this many elements
  -> [(a, Maybe Bool)]
  -> ([(a, Maybe Bool)], Int)
takeCount i = flip St.runState 0 . unfoldrM f
  where
    f [] = return Nothing
    f ((a, mb):xs) = do
      c <- St.get
      case () of
        _ | c == i -> return Nothing
          | isTrue mb -> do
              St.put (c + 1)
              return (Just ((a, mb), xs))
          | otherwise -> return (Just ((a, mb), xs))


-- | Determines whether to show a subject, and shows it.
showSubject
  :: (a -> X.Text)
  -> Verbosity
  -> Pt.IndentAmt
  -> Pt.Level
  -> Pt.Pdct a
  -> (a, Maybe Bool)
  -> [C.Chunk]
showSubject swr v i l p (s, b) =
  let (showSubj, showDisc) = isSubjectAndDiscardsShown v b
      renamer txt = X.concat [swr s, " - ", txt]
      renamed = Pt.rename renamer p
  in if showSubj
     then snd $ Pt.evaluate i showDisc s l renamed
     else []

-- | Given a Verbosity and a Maybe Boolean indicating whether a
-- subject is True, False, or a discard, returns whether to show the
-- subject and whether to show the discards contained within the
-- subject.
isSubjectAndDiscardsShown :: Verbosity -> Maybe Bool -> (Bool, Bool)
isSubjectAndDiscardsShown v b = case v of
  Silent -> (False, False)
  PassFail -> (False, False)
  FalseSubjects -> (not . isTrue $ b, False)
  TrueSubjects -> (isJust b, False)
  DiscardedSubjects -> (True, False)
  DiscardedPredicates -> (True, True)


showTestTitle :: Pt.IndentAmt -> Pt.Level -> Name -> Pass -> [C.Chunk]
showTestTitle i l n p = [idt, open, passFail, close, blank, txt, nl]
  where
    passFail = C.chunk ts tf
    idt = C.chunk C.defaultTextSpec (X.replicate (i * l) " ")
    nl = C.chunk C.defaultTextSpec "\n"
    (tf, ts) =
      if p
      then ("PASS", Sw.switchForeground C.color8_f_green
                    C.color256_f_2 C.defaultTextSpec)
      else ("FAIL", Sw.switchForeground C.color8_f_red
                    C.color256_f_1 C.defaultTextSpec)
    open = C.chunk C.defaultTextSpec "["
    close = C.chunk C.defaultTextSpec "]"
    blank = C.chunk C.defaultTextSpec (X.singleton ' ')
    txt = C.chunk C.defaultTextSpec n

isTrue :: Maybe Bool -> Bool
isTrue = maybe False id

--
-- Tests
--

-- | Passes if every subject is True.
eachSubjectMustBeTrue
  :: Name
  -> (a -> Text)
  -> Pt.Pdct a
  -> Tree a
eachSubjectMustBeTrue n swr p = Tree n (Test tf)
  where
    tf i pv fv as lvl = (pass, cks)
      where
        rslts = zip as (map (Pt.eval p) as)
        pass = all (isTrue . snd) rslts
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle i lvl n pass
        subjectChunks =
          concatMap (showSubject swr v i (lvl + 1) p) rslts

-- | Passes if at least n subjects are True.
seriesAtLeastN
  :: Name
  -> (a -> X.Text)
  -> Int
  -> Pt.Pdct a
  -> Tree a
seriesAtLeastN n swr i p = Tree n (Test tf)
  where
    tf i pv fv as l = (pass, cks)
      where
        (elems, nFound) = takeCount i (zip as (map (Pt.eval p) as))
        pass = nFound >= i
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle i l n pass
        subjectChunks =
          concatMap (showSubject swr v i (l + 1) p) elems

{-
------------------------------------------------------------
-- Series
------------------------------------------------------------


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
