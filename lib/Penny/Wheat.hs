{-# LANGUAGE OverloadedStrings #-}

-- | Wheat - Penny ledger tests
--
-- Wheat helps you build tests to check all the postings in your
-- ledger. Perhaps you want to make sure all the account names are
-- valid, or that your checking account has no unreconciled
-- transactions. With Wheat you can easily build a command line
-- program that will check all the postings in a ledger for you
-- against criteria that you specify.

module Penny.Wheat
  ( -- * Configuration
    WheatConf(..)

    -- * Tests
  , eachPostingMustBeTrue
  , atLeastNPostings

    -- * Convenience functions
  , futureFirstsOfTheMonth

    -- * Running tests
  , main
  ) where

import Control.Monad (when)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Text.Matchers as M
import qualified Text.Parsec as Parsec
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Penny.Shield as S
import qualified Penny.Steel.Sums as Su

import qualified Data.Version as V
import qualified Data.Prednote.TestTree as TT
import qualified Data.Prednote.Pdct as Pe
import qualified System.Console.Rainbow as Rb
import qualified System.Console.MultiArg as MA
import System.Locale (defaultTimeLocale)

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

-- | Record holding all data to configure Wheat.
data WheatConf = WheatConf
  { briefDescription :: String
    -- ^ This is displayed at the beginning of the online help. It
    -- should be a one-line description of what this program does--for
    -- example, what it checks for.

  , moreHelp :: [String]
    -- ^ Displayed at the end of the online help. It should be a list
    -- of lines, wich each line not terminated by a newline
    -- character. It is displayed at the end of the online help.

  , tests :: [Time.UTCTime -> TT.TestTree L.Posting]
    -- ^ The actual tests to run. The UTCTime is the @base time@. Each
    -- test may decide what to do with the base time--for example, the
    -- test might say that all postings have to have a date on or
    -- before that date. Or the test might just ignore the base time.

  , indentAmt :: Pe.IndentAmt
    -- ^ How many spaces to indent each level in a tree of tests.

  , passVerbosity :: TT.Verbosity
    -- ^ Verbosity for tests that pass

  , failVerbosity :: TT.Verbosity
    -- ^ Verbosity for tests that fail

  , groupPred :: TT.Name -> Bool
    -- ^ Group names are filtered with this function; a group is only
    -- run if this function returns True.

  , testPred :: TT.Name -> Bool
    -- ^ Test names are filtered with this function; a test is only
    -- run if this function returns True.

  , showSkippedTests :: Bool
    -- ^ Some tests might be skipped; see 'testPred'. This controls
    -- whether you want to see a notification of tests that were
    -- skipped. (Does not affect skipped groups; see 'groupVerbosity'
    -- for that.)

  , groupVerbosity :: TT.GroupVerbosity
    -- ^ Show group names? Even if you do not show the names of
    -- groups, tests within the group will still be indented.

  , stopOnFail :: Bool
    -- ^ If True, then tests will stop running immediately after a
    -- single test fails. If False, all tests are always run.

  , colorToFile :: Bool
    -- ^ Use colors even if stdout is not a file?

  , baseTime :: Time.UTCTime
    -- ^ Tests may use this date and time as they wish; see
    -- 'tests'. Typically you will set this to the current instant.

  , ledgers :: [String]
    -- ^ Ledger files to read in from disk.
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
  , p_colorToFile :: Bool
  , p_baseTime :: Time.UTCTime
  , p_help :: Bool
  , p_ledgers :: [String]
  }

parseBaseTime :: String -> Ex.Exceptional MA.InputError Time.UTCTime
parseBaseTime s = case Parsec.parse CP.dateTime  "" (X.pack s) of
  Left e -> Ex.throw (MA.ErrorMsg $ "could not parse date: " ++ show e)
  Right g -> return . L.toUTC $ g

parseRegexp :: String -> Ex.Exceptional MA.InputError (TT.Name -> Bool)
parseRegexp s = case M.pcre M.Sensitive (X.pack s) of
  Ex.Exception e -> Ex.throw . MA.ErrorMsg $
    "could not parse regular expression: " ++ X.unpack e
  Ex.Success m -> return . M.match $ m

parseArg :: String -> Parsed -> Parsed
parseArg s p = p { p_ledgers = p_ledgers p ++ [s] }

allOpts :: [MA.OptSpec (Parsed -> Parsed)]
allOpts =
  let allChoices =
        [ ("silent", \p -> p { p_failVerbosity = TT.Silent })
        , ("minimal", \p -> p { p_failVerbosity = TT.PassFail })
        , ("false", \p -> p { p_failVerbosity = TT.FalseSubjects })
        , ("true", \p -> p { p_failVerbosity = TT.TrueSubjects })
        , ("all", \p -> p { p_failVerbosity = TT.Discards })
        ] in
  [ MA.OptSpec ["indentation"] "i"
    (fmap (\i p -> p { p_indentAmt = i }) (MA.OneArgE MA.reader))

  , MA.OptSpec ["pass-verbosity"] "p" $ MA.ChoiceArg allChoices

  , MA.OptSpec ["fail-verbosity"] "f" $ MA.ChoiceArg allChoices

  , MA.OptSpec ["group-regexp"] "g"
    (fmap (\f p -> p { p_groupPred = f }) (MA.OneArgE parseRegexp))

  , MA.OptSpec ["test-regexp"] "t"
    (fmap (\f p -> p { p_testPred = f }) (MA.OneArgE parseRegexp))

  , MA.OptSpec ["show-skipped-tests"] ""
    ( MA.NoArg (\p -> p { p_showSkippedTests
                          = not (p_showSkippedTests p) }))

  , MA.OptSpec ["group-verbosity"] "G" $ MA.ChoiceArg
    [ ("silent", \p -> p { p_groupVerbosity = TT.NoGroups })
    , ("active", \p -> p { p_groupVerbosity = TT.ActiveGroups })
    , ("all", \p -> p { p_groupVerbosity = TT.AllGroups })
    ]

  , MA.OptSpec ["stop-on-failure"] ""
    ( MA.NoArg (\p -> p { p_stopOnFail
                          = not (p_stopOnFail p) }))

  , MA.OptSpec ["color-to-file"] ""
    ( MA.NoArg (\p -> p { p_colorToFile
                          = not (p_colorToFile p) }))

  , MA.OptSpec ["base-date"] ""
    (fmap (\d p -> p { p_baseTime = d }) (MA.OneArgE parseBaseTime))
  ]

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

-- | Runs Wheat tests. Prints the result to standard output. Exits
-- unsuccessfully if the user gave bad command line options or if at
-- least a single test failed; exits successfully if all tests
-- succeeded. Shows the version number and exits successfully if that
-- was requested.
main
  :: V.Version
  -- ^ Version of the binary
  -> (S.Runtime -> WheatConf) -> IO ()
main ver getWc = do
  rt <- S.runtime
  let wc = getWc rt
  parsed <- MA.simpleWithHelp (help wc) MA.Intersperse
         (fmap Left (Ly.version ver) : (map (fmap Right) allOpts))
         (return . (fmap Right parseArg))
  let (showVers, fns) = partitionEithers parsed
  case showVers of
    [] -> return ()
    x:_ -> x
  let fn = foldl (flip (.)) id fns
      psd = fn (getParsedFromWheatConf wc)
  term <- Rb.smartTermFromEnv (p_colorToFile psd) IO.stdout
  pfs <- getItems (p_ledgers psd)
  let ttOpts = getTTOpts pfs psd
      tts = zipWith ($) (tests wc) (repeat (p_baseTime psd))
      (cks, _, nFail) = TT.runTests ttOpts 0 tts
  Rb.putChunks term cks
  when (nFail > 0) Exit.exitFailure

getParsedFromWheatConf :: WheatConf -> Parsed
getParsedFromWheatConf w = Parsed
  { p_indentAmt = indentAmt w
  , p_passVerbosity = passVerbosity w
  , p_failVerbosity = failVerbosity w
  , p_groupPred = groupPred w
  , p_testPred = testPred w
  , p_showSkippedTests = showSkippedTests w
  , p_groupVerbosity = groupVerbosity w
  , p_stopOnFail = stopOnFail w
  , p_colorToFile = colorToFile w
  , p_baseTime = baseTime w
  , p_help = False
  , p_ledgers = ledgers w
  }

getItems :: [String] -> IO [L.Posting]
getItems ss = fmap f $ Cop.open ss
  where
    f = concatMap L.transactionToPostings
        . mapMaybe ( let cn = const Nothing
                     in Su.caseS4 Just cn cn cn)

--
-- Tests
--

-- | Passes only if each posting is True.
eachPostingMustBeTrue
  :: TT.Name
  -> Pe.Pdct L.Posting
  -> TT.TestTree L.Posting
eachPostingMustBeTrue n = TT.eachSubjectMustBeTrue n L.display

-- | Passes if at least a particular number of postings is True.
atLeastNPostings
  :: Int
  -- ^ The number of postings that must be true for the test to pass
  -> TT.Name
  -> Pe.Pdct L.Posting
  -> TT.TestTree L.Posting
atLeastNPostings i n = TT.nSubjectsMustBeTrue n L.display i

--
-- Help
--

help
  :: WheatConf
  -> String
  -- ^ Program name
  -> String
help wc pn = unlines
  [ "usage: " ++ pn ++ " [options] [FILE...]"
  , ""
  , briefDescription wc
  , ""
  , "Options:"
  , "  -i, --indentation AMT"
  , "    Indent each level by this many spaces"
  , "    " ++ dflt (show . indentAmt $ wc)
  , "  -p, --pass-verbosity VERBOSITY"
  , "    Verbosity for tests that pass. Argument may be:"
  , "      silent - show nothing at all"
  , "      minimal - show whether the test passed or failed"
  , "      false - show subjects that are false"
  , "      true - show subjects that are true or false"
  , "      all - show all subjects"
  , "      " ++ dflt (showVerbosity . passVerbosity $ wc)
  , "  -f, --fail-verbosity VERBOSITY"
  , "    Verbosity for tests that fail."
  , "    (uses same VERBOSITY options as --pass-verbosity)"
  , "    " ++ dflt (showVerbosity . failVerbosity $ wc)
  , "  -g, --group-regexp REGEXP"
  , "    Run only groups whose name matches the given"
  , "    Perl-compatible regular expression"
  , "    (overrides the compiled-in default)"
  , "  -t, --test-regexp REGEXP"
  , "    Run only tests whose name matches the given"
  , "    Perl-compatible regular expression"
  , "    (overrides the compiled-in default)"
  , "  --show-skipped-tests"
  , "    Toggle whether to show tests that are skipped"
  , "    using the --test-regexp option"
  , "    (does not affect groups that are skipped; see next option)"
  , "    " ++ dflt (show . showSkippedTests $ wc)
  , "  --G, group-verbosity ARG"
  , "    Control which group names are shown. Argument may be:"
  , "      silent - do not show any group names"
  , "      active - show group names that were not skipped"
  , "      all - show all group names, including skipped ones"
  , "      " ++ dflt (showGroupVerbosity . groupVerbosity $ wc)
  , "  --stop-on-failure"
  , "    Stop running tests after a single test fails"
  , "    " ++ dflt (show . stopOnFail $ wc)
  , "  --color-to-file"
  , "    Use color even when standard output is not a terminal"
  , "    " ++ dflt (show . colorToFile $ wc)
  , "  --base-date DATE"
  , "    Use this date as a basis for checks"
  , "    " ++ dflt ( Time.formatTime defaultTimeLocale "%c"
                     . baseTime $ wc)
  , ""
  ]
  ++ unlines (moreHelp wc)

dflt :: String -> String
dflt s = "(default: " ++ s ++ ")"

showVerbosity :: TT.Verbosity -> String
showVerbosity v = case v of
  TT.Silent -> "silent"
  TT.PassFail -> "minimal"
  TT.FalseSubjects -> "false"
  TT.TrueSubjects -> "true"
  TT.Discards -> "all"

showGroupVerbosity :: TT.GroupVerbosity -> String
showGroupVerbosity v = case v of
  TT.NoGroups -> "silent"
  TT.ActiveGroups -> "active"
  TT.AllGroups -> "all"


