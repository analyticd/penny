module Penny.Wheat
  ( -- * Basic predicates
    Pdct
  , payee
  , number
  , flag
  , postingMemo
  , transactionMemo
  , dateTime
  , CompQty(..)
  , qty
  , drCr
  , commodity
  , account
  , accountLevel
  , accountAny
  , tag

  -- * Convenience predicates
  , reconciled

  -- * Other conveniences
  , futureFirstsOfTheMonth

  -- * Configuration and CLI
  , ColorToFile
  , BaseTime
  , WheatConf(..)
  , wheatMain

  -- * Re-exports
  , S.Runtime
  , S.currentTime
  , module Text.Matchers
  , module Penny.Steel.Prednote
  , module Data.Time
  ) where

import Control.Arrow (second)
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
import qualified Data.Time as T
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
import Data.Time

type Pdct = N.Pdct L.PostFam

-- * Pattern matching fields

descItem :: String -> M.Matcher -> String
descItem s m = "field: " ++ s
               ++ "; matcher description: "
               ++ (X.unpack $ M.matchDesc m)

payee :: M.Matcher -> Pdct
payee p = pdct (descItem "payee" p) (P.payee (M.match p))


number :: M.Matcher -> Pdct
number p = pdct (descItem "number" p) (P.number (M.match p))

flag :: M.Matcher -> Pdct
flag p = pdct (descItem "flag" p) (P.flag (M.match p))

postingMemo :: M.Matcher -> Pdct
postingMemo p = pdct (descItem "posting memo" p)
                (P.postingMemo (M.match p))

transactionMemo :: M.Matcher -> Pdct
transactionMemo p = pdct (descItem "transaction memo" p)
                (P.transactionMemo (M.match p))

-- * UTC times

dateTime :: M.CompUTC -> T.UTCTime -> Pdct
dateTime c t = pdct ("posting " ++ M.descUTC c t) p
  where
    p = (`cmp` t) . L.toUTC . Q.dateTime
    cmp = M.compUTCtoCmp c

-- * Quantities

data CompQty
  = QGT
  | QGTEQ
  | QEQ
  | QLTEQ
  | QLT

descQty :: CompQty -> L.Qty -> String
descQty c q = "quantity of posting is " ++ co ++ " " ++ show q
  where
    co = case c of
      QGT -> "greater than"
      QGTEQ -> "greater than or equal to"
      QEQ -> "equal to"
      QLTEQ -> "less than or equal to"
      QLT -> "less than"

qty :: CompQty -> L.Qty -> Pdct
qty c q = pdct (descQty c q) p
  where
    p = (`cmp` q) . Q.qty
    cmp = case c of
      QGT -> (>)
      QGTEQ -> (>=)
      QEQ -> (==)
      QLTEQ -> (<=)
      QLT -> (<)

-- * DrCr

drCr :: L.DrCr -> Pdct
drCr dc = pdct ("posting is a " ++ desc) p
  where
    desc = case dc of
      L.Debit -> "debit"
      L.Credit -> "credit"
    p = (== dc) . Q.drCr

-- * Commodity

commodity :: M.Matcher -> Pdct
commodity p = pdct (descItem "commodity" p) (P.commodity (M.match p))

-- * Account name

account :: M.Matcher -> Pdct
account p = pdct (descItem "full account name" p)
            (P.account (X.singleton ':') (M.match p))

accountLevel :: Int -> M.Matcher -> Pdct
accountLevel i p = pdct (descItem ("sub-account " ++ show i) p)
                   (P.accountLevel i (M.match p))

accountAny :: M.Matcher -> Pdct
accountAny p = pdct (descItem "any sub-account" p)
               (P.accountAny (M.match p))

-- * Tags

tag :: M.Matcher -> Pdct
tag p = pdct (descItem "any tag" p) (P.tag (M.match p))

------------------------------------------------------------
-- Convenience predicates
------------------------------------------------------------

-- | True if a posting is reconciled.
reconciled :: Pdct
reconciled =
  pdct "posting flag is exactly \"R\" (is reconciled)"
  (maybe False ((== X.singleton 'R'). L.unFlag) . Q.flag)

------------------------------------------------------------
-- Other conveniences
------------------------------------------------------------


-- | A non-terminating list of starting with the first day of the
-- first month following the given day, followed by successive first
-- days of the month.
futureFirstsOfTheMonth :: T.Day -> [T.Day]
futureFirstsOfTheMonth d = iterate (T.addGregorianMonthsClip 1) d1
  where
    d1 = T.fromGregorian yr mo 1
    (yr, mo, _) = T.toGregorian $ T.addGregorianMonthsClip 1 d

------------------------------------------------------------
-- CLI
------------------------------------------------------------

type ProgName = String
type BriefDesc = String
type ColorToFile = Bool
type BaseTime = L.DateTime
type MoreHelp = [String]

help
  :: ProgName
  -> BriefDesc
  -> N.Verbosity
  -> N.SpaceCount
  -> ColorToFile
  -> BaseTime
  -> MoreHelp
  -> String
help pn bd v sc ctf bt mh = unlines $
  [ "usage: " ++ pn ++ "[options] ARGS"
  , ""
  , bd
  , "Options:"
  , ""
  , "--color-to-file no|yes"
  , "  If yes, use colors even when standard output is"
  , "  not a terminal. (default: " ++ dCtf ++ ")"
  , ""
  , "--verbosity, -v VERBOSITY"
  , "  Use the given level of verbosity. Choices:"
  , "    silent - show nothing at all"
  , "    fails - show only series that fail"
  , "    brief - show only whether each series succeeded or failed"
  , "    interesting - show interesting result from failed"
  , "      series; for successful series, show only that they"
  , "      succeeded"
  , "    allFails - show all result from failed series; for"
  , "      successful series, show only that they succeeded"
  , "    everything - show all results from all series"
  , "    (default: " ++ dVerb ++ ")"
  , ""
  , "--indentation, -i SPACES"
  , "  indent each level by this many spaces"
  , "  (default: " ++ dSc ++ ")"
  , ""
  , "--base-date, -d DATE"
  , "  use this date as basis for checks"
  , "  (currently: " ++ showDateTime bt ++ ")"
  , ""
  , "--help, -h - show help and exit"
  , ""
  ] ++ mh
  where
    dCtf = if ctf then "yes" else "no"
    dVerb = case v of
      N.Silent -> "silent"
      N.FailOnly -> "fails"
      N.Brief -> "brief"
      N.InterestingFails -> "interesting"
      N.AllFails -> "allFails"
      N.AllAll -> "everything"
    dSc = show sc

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
  = AHelp
  | AVerbosity N.Verbosity
  | AColorToFile ColorToFile
  | AIndentation N.SpaceCount
  | ABaseTime L.DateTime
  | APosArg String
  deriving Eq

optHelp :: MA.OptSpec Arg
optHelp = MA.OptSpec ["help"] "h" (MA.NoArg AHelp)

optVerbosity :: MA.OptSpec Arg
optVerbosity = MA.OptSpec ["verbosity"] "v" (MA.ChoiceArg ls)
  where
    ls = fmap (second AVerbosity) $
         [ ("silent", N.Silent)
         , ("fails", N.FailOnly)
         , ("brief", N.Brief)
         , ("interesting", N.InterestingFails)
         , ("allFails", N.AllFails)
         , ("everything", N.AllAll)
         ]

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

type ParsedOpts = ( N.Verbosity, N.SpaceCount,
                    ColorToFile, BaseTime, [String])

data ParseResult
  = NeedsHelp
  | ParseErr String
  | Parsed ParsedOpts

-- | When passed the defaults, return the values to use, as they might
-- have been affected by the command arguments, or return Nothing if
-- help is needed.
parseArgs
  :: N.Verbosity
  -> N.SpaceCount
  -> ColorToFile
  -> BaseTime
  -> [String]
  -> ParseResult
parseArgs v sc ctf bt ss =
  let exLs = MA.simple MA.Intersperse opts (return . APosArg) ss
      opts = [ fmap return optHelp
             , fmap return optVerbosity
             , fmap return optColorToFile
             , optIndentation
             , optBaseTime
             ]
  in case exLs of
      Ex.Exception e -> ParseErr . show $ e
      Ex.Success ls -> case sequence ls of
        Ex.Exception e -> ParseErr e
        Ex.Success ls' ->
          if AHelp `elem` ls'
          then NeedsHelp
          else Parsed ( (getVerbosity v ls'), (getSpaceCount sc ls')
                        , (getColorToFile ctf ls'), (getBaseTime bt ls')
                        , (getPosArg ls'))

getVerbosity :: N.Verbosity -> [Arg] -> N.Verbosity
getVerbosity v as = case mapMaybe f as of
  [] -> v
  xs -> last xs
  where f a = case a of { AVerbosity vb -> Just vb; _ -> Nothing }

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
  , verbosity :: N.Verbosity
  , spaceCount :: N.SpaceCount
  , colorToFile :: ColorToFile
  , groups :: BaseTime -> [N.SeriesGroup L.PostFam]
  , baseTime :: BaseTime
  }

applyParse
  :: ProgName
  -> WheatConf
  -> [String]
  -> IO ParsedOpts
applyParse pn c as =
  case parseArgs (verbosity c) (spaceCount c)
       (colorToFile c) (baseTime c) as of
    NeedsHelp -> do
      putStrLn (help pn (briefDescription c)
                (verbosity c) (spaceCount c) (colorToFile c) (baseTime c)
                (moreHelp c))
      Exit.exitSuccess
    ParseErr e -> do
      putStrLn $ pn ++ ": could not parse command line: " ++ e
      Exit.exitFailure
    Parsed r -> return r

wheatMain :: (S.Runtime -> WheatConf) -> IO ()
wheatMain getConf = do
  rt <- S.runtime
  pn <- MA.getProgName
  as <- MA.getArgs
  let c = getConf rt
  (vbsty, sc, ctf, bt, posargs) <- applyParse pn c as
  let getTerm =
        if ctf || (S.output rt == S.IsTTY)
        then TI.setupTermFromEnv
        else TI.setupTerm "dumb"
  ti <- getTerm
  items <- getItems pn posargs
  let srs = map (N.runSeries items) (groups c bt)
  rList <- return $! map N.resultList srs
  mapM_ (N.showSeries ti display sc vbsty) srs
  if and $ concat rList
    then Exit.exitSuccess
    else Exit.exitFailure

-- | Displays a PostFam in a one line format.
--
-- Format:
--
-- File LineNo Date Payee Acct DrCr Cmdty Qty
display :: L.PostFam -> String
display p = concat (intersperse " " ls)
  where
    ls = [file, lineNo, dt, pye, acct, dc, cmdty, qt]
    file = maybe (labelNo "filename") (X.unpack . L.unFilename)
           (Q.filename p)
    lineNo = maybe (labelNo "line number")
             (show . L.unPostingLine) (Q.postingLine p)
    dateFormat = "%Y-%m-%d %T %z"
    dt = T.formatTime defaultTimeLocale dateFormat
         . T.utctDay
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
