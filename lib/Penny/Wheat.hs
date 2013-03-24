{-# LANGUAGE OverloadedStrings #-}
module Penny.Wheat where

import Control.Applicative
import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>), mempty)
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Text.Matchers as M
import qualified Text.Parsec as Parsec
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Penny.Shield as S

import qualified Data.Prednote.TestTree as TT
import qualified Data.Prednote.Pdct as Pe
import qualified System.Console.Rainbow as Rb
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
  , tests :: [BaseTime -> TT.TestTree L.PostFam]
  , indentAmt :: Pe.IndentAmt
  , passVerbosity :: TT.Verbosity
  , failVerbosity :: TT.Verbosity
  , groupPred :: TT.Name -> Bool
  , testPred :: TT.Name -> Bool
  , showSkippedTests :: Bool
  , groupVerbosity :: TT.GroupVerbosity
  , stopOnFail :: Bool
  , colorToFile :: ColorToFile
  , baseTime :: BaseTime
  , ledgers :: [String]
  }

data Parsed = Parsed
  { p_indentAmt :: Pe.IndentAmt
  , p_passVerbosity :: TT.Verbosity
  , p_failVerbosity :: TT.Verbosity
  , p_groupPred :: TT.Name -> Bool
  , p_testPred :: TT.Name -> Bool
  , p_showSkippedTests :: Bool
  , p_groupVerbosity :: TT.GroupVerbosity
  , p_stopOnFail :: Bool
  , p_colorToFile :: ColorToFile
  , p_baseTime :: BaseTime
  , p_ledgers :: [String]
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
  , ("all", TT.Discards)
  ]

parseColorToFile :: String -> Either OA.ParseError ColorToFile
parseColorToFile = parseAbbrev [ ("no", False), ("yes", True) ]

parseBaseTime :: String -> Either OA.ParseError BaseTime
parseBaseTime s = case Parsec.parse CP.dateTime  "" (X.pack s) of
  Left e -> Left (OA.ErrorMsg $ "could not parse date: " ++ show e)
  Right g -> Right . L.toUTC $ g

parseRegexp :: String -> Either OA.ParseError (TT.Name -> Bool)
parseRegexp s = case M.pcre M.Sensitive (X.pack s) of
  Ex.Exception e -> Left . OA.ErrorMsg $
    "could not parse regular expression: " ++ X.unpack e
  Ex.Success m -> Right . M.match $ m

parseGroupVerbosity :: String -> Either OA.ParseError TT.GroupVerbosity
parseGroupVerbosity = parseAbbrev
  [ ("silent", TT.NoGroups)
  , ("active", TT.ActiveGroups)
  , ("all", TT.AllGroups)
  ]

parseOpts :: WheatConf -> OA.Parser Parsed
parseOpts wc
  = Parsed
  <$> ( OA.option
        ( OA.long "indentation"
        <> OA.short 'i' )
      <|> pure (indentAmt wc) )

  <*> ( OA.nullOption
        ( OA.long "pass-verbosity"
        <> OA.short 'p'
        <> OA.reader parseVerbosity )
      <|> pure (passVerbosity wc) )

  <*> ( OA.nullOption
        ( OA.long "fail-verbosity"
        <> OA.short 'f'
        <> OA.reader parseVerbosity )
      <|> pure (failVerbosity wc) )

  <*> ( OA.nullOption
        ( OA.long "group-regexp"
          <> OA.short 'g'
          <> OA.reader parseRegexp )
      <|> pure (groupPred wc) )

  <*> ( OA.nullOption
        ( OA.long "test-regexp"
          <> OA.short 't'
          <> OA.reader parseRegexp )
      <|> pure (testPred wc) )

  <*> ( OA.flag (showSkippedTests wc) False
        ( OA.long "show-skipped-tests" ))

  <*> ( OA.nullOption
        ( OA.long "group-verbosity"
          <> OA.short 'G'
          <> OA.reader parseGroupVerbosity )
        <|> pure (groupVerbosity wc))

  <*> ( OA.flag (stopOnFail wc) True (OA.long "stop-on-failure"))

  <*> ( OA.nullOption
        ( OA.long "color-to-file"
        <> OA.reader parseColorToFile )
      <|> pure (colorToFile wc))

  <*> ( OA.nullOption
        ( OA.long "base-date"
        <> OA.reader parseBaseTime )
      <|> pure (baseTime wc) )

  <*> ( some (OA.argument OA.str mempty) <|> pure (ledgers wc))

getTTOpts :: [a] -> Parsed -> TT.TestOpts a
getTTOpts as o = TT.TestOpts
  { TT.tIndentAmt = p_indentAmt o
  , TT.tPassVerbosity = p_passVerbosity o
  , TT.tFailVerbosity = p_failVerbosity o
  , TT.tGroupPred = p_groupPred o
  , TT.tTestPred = p_testPred o
  , TT.tShowSkippedTests = p_showSkippedTests o
  , TT.tGroupVerbosity = p_groupVerbosity o
  , TT.tSubjects = as
  , TT.tStopOnFail = p_stopOnFail o
  }

main :: (S.Runtime -> WheatConf) -> IO ()
main getWc = do
  rt <- S.runtime
  let wc = getWc rt
  psd <- OA.execParser (OA.info (parseOpts (getWc rt)) OA.fullDesc)
  term <- Rb.smartTermFromEnv (p_colorToFile psd) IO.stdout
  pfs <- getItems (p_ledgers psd)
  let ttOpts = getTTOpts pfs psd
  let tts = zipWith ($) (tests wc) (repeat (p_baseTime psd))
      (cks, _, nFail) = TT.runTests ttOpts 0 tts
  Rb.printChunks term cks
  when (nFail > 0) Exit.exitFailure

getItems :: [String] -> IO [L.PostFam]
getItems ss = fmap f $ Cop.open ss
  where
    f = concatMap L.postFam . mapMaybe toTxn . Cop.unLedger
    toTxn i = case i of { Cop.Transaction x -> Just x; _ -> Nothing }

--
-- Tests
--
eachPostingMustBeTrue
  :: TT.Name
  -> Pe.Pdct L.PostFam
  -> TT.TestTree L.PostFam
eachPostingMustBeTrue n = TT.eachSubjectMustBeTrue n L.display

atLeastNPostings
  :: Int
  -> TT.Name
  -> Pe.Pdct L.PostFam
  -> TT.TestTree L.PostFam
atLeastNPostings i n = TT.nSubjectsMustBeTrue n L.display i
