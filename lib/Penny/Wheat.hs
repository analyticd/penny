{-# LANGUAGE OverloadedStrings #-}
module Penny.Wheat where

import Control.Applicative
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (intersperse, find, isPrefixOf)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Monoid ((<>), mempty)
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Text.Parsec as Parsec
import qualified System.Exit as Exit
import System.Locale (defaultTimeLocale)
import System.Environment (getProgName)
import qualified System.IO as IO
import qualified Penny.Shield as S

import qualified Penny.Steel.TestTree as TT
import qualified Penny.Steel.Pdct as Pe
import qualified Penny.Steel.Chunk as C
import qualified Options.Applicative as OA

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

type ColorToFile = Bool
type BaseTime = Time.UTCTime
type ProgName = String

data WheatConf = WheatConf
  { briefDescription :: String
  , moreHelp :: [String]
  , passVerbosity :: TT.PassVerbosity
  , failVerbosity :: TT.FailVerbosity
  , indentAmt :: Pe.IndentAmt
  , colorToFile :: ColorToFile
  , tests :: [BaseTime -> TT.TestTree L.PostFam]
  , baseTime :: BaseTime
  }

parseAbbrev :: [(String, a)] -> String -> Either OA.ParseError a
parseAbbrev ls str = case find (\(s, _) -> s == str) ls of
  Nothing -> lookupAbbrev
  Just (_, a) -> Right a
  where
    lookupAbbrev = case filter ((str `isPrefixOf`) . fst) ls of
      (_, a):[] -> Right a
      _ -> Left (OA.ErrorMsg ("invalid argument: " ++ str))

parseVerbosity :: String -> Either OA.ParseError TT.Verbosity
parseVerbosity = parseAbbrev
  [ ("silent", TT.Silent)
  , ("minimal", TT.PassFail)
  , ("false", TT.FalseSubjects)
  , ("true", TT.TrueSubjects)
  , ("discarded", TT.DiscardedSubjects)
  , ("all", TT.DiscardedPredicates)
  ]

parseColorToFile :: String -> Either OA.ParseError ColorToFile
parseColorToFile = parseAbbrev [ ("no", False), ("yes", True) ]

parseBaseTime :: String -> Either OA.ParseError BaseTime
parseBaseTime s = case Parsec.parse CP.dateTime  "" (X.pack s) of
  Left e -> Left (OA.ErrorMsg $ "could not parse date: " ++ show e)
  Right g -> Right . L.toUTC $ g

data Parsed = Parsed
  { p_passVerbosity :: TT.PassVerbosity
  , p_failVerbosity :: TT.FailVerbosity
  , p_indentAmt :: Pe.IndentAmt
  , p_colorToFile :: ColorToFile
  , p_baseTime :: BaseTime
  , p_ledgers :: [String]
  }

parseOpts :: WheatConf -> OA.Parser Parsed
parseOpts wc
  = Parsed
  <$> ( OA.nullOption
        ( OA.long "pass-verbosity"
        <> OA.short 'p'
        <> OA.reader parseVerbosity )
      <|> pure (passVerbosity wc) )

  <*> ( OA.nullOption
        ( OA.long "fail-verbosity"
        <> OA.short 'f'
        <> OA.reader parseVerbosity )
      <|> pure (failVerbosity wc) )

  <*> ( OA.option
        ( OA.long "indentation"
        <> OA.short 'i' )
      <|> pure (indentAmt wc) )

  <*> ( OA.nullOption
        ( OA.long "color-to-file"
        <> OA.reader parseColorToFile )
      <|> pure (colorToFile wc))

  <*> ( OA.nullOption
        ( OA.long "base-date"
        <> OA.reader parseBaseTime )
      <|> pure (baseTime wc) )

  <*> ( many (OA.argument OA.str mempty))

main :: (S.Runtime -> WheatConf) -> IO ()
main getWc = do
  pn <- getProgName
  rt <- S.runtime
  let inf = OA.fullDesc
      wc = getWc rt
  psd <- OA.execParser (OA.info (parseOpts wc) inf)
  let term = if p_colorToFile psd || (S.output rt == S.IsTTY)
        then S.termFromEnv rt
        else S.autoTerm rt
  pfs <- getItems pn (p_ledgers psd)
  let tts = zipWith ($) (tests wc) (repeat (p_baseTime psd))
      doEval = TT.evalTestTree (p_indentAmt psd) 0 (p_passVerbosity psd)
                           (p_failVerbosity psd) pfs
      eithers = concatMap doEval tts
  passes <- mapM (showEitherChunk (C.printChunks term)) eithers
  when (not . and . catMaybes $ passes) Exit.exitFailure

showEitherChunk
  :: ([C.Chunk] -> IO ())
  -> Either C.Chunk (TT.Pass, [C.Chunk])
  -> IO (Maybe TT.Pass)
showEitherChunk f ei = case ei of
  Left ck -> f [ck] >> return Nothing
  Right (p, cs) -> f cs >> return (Just p)

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
--
-- Showers
--
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

--
-- Tests
--
eachPostingMustBeTrue
  :: TT.Name
  -> Pe.Pdct L.PostFam
  -> TT.TestTree L.PostFam
eachPostingMustBeTrue n = TT.eachSubjectMustBeTrue n display

atLeastNPostings
  :: Int
  -> TT.Name
  -> Pe.Pdct L.PostFam
  -> TT.TestTree L.PostFam
atLeastNPostings i n = TT.seriesAtLeastN n display i
