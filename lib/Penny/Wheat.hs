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
import qualified Data.Prednote.Test as TT
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

  , tests :: [Time.UTCTime -> TT.Test L.Posting]
    -- ^ The actual tests to run. The UTCTime is the @base time@. Each
    -- test may decide what to do with the base time--for example, the
    -- test might say that all postings have to have a date on or
    -- before that date. Or the test might just ignore the base time.

  , indentAmt :: Pe.IndentAmt
    -- ^ How many spaces to indent each level in a tree of tests.

  , verbosity :: Maybe TT.TestVerbosity
    -- ^ If Just, use this verbosity. If Nothing, use the default
    -- verbosity provided by the tests themselves.

  , testPred :: TT.Name -> Bool
    -- ^ Test names are filtered with this function; a test is only
    -- run if this function returns True.

  , stopOnFail :: Bool
    -- ^ If True, then tests will stop running immediately after a
    -- single test fails. If False, all tests are always run.

  , colorToFile :: Bool
    -- ^ Use colors even if stdout is not a file?

  , baseTime :: Time.UTCTime
    -- ^ Tests may use this date and time as they wish; see
    -- 'tests'. Typically you will set this to the current instant.

  , formatQty :: [Cop.LedgerItem] -> L.Amount L.Qty -> X.Text
  -- ^ How to format quantities

  }

parseBaseTime :: String -> Either MA.InputError Time.UTCTime
parseBaseTime s = case Parsec.parse CP.dateTime  "" (X.pack s) of
  Left e -> Left (MA.ErrorMsg $ "could not parse date: " ++ show e)
  Right g -> return . L.toUTC $ g

parseRegexp :: String -> Either MA.InputError (TT.Name -> Bool)
parseRegexp s = case M.pcre M.Sensitive (X.pack s) of
  Left e -> Left . MA.ErrorMsg $
    "could not parse regular expression: " ++ X.unpack e
  Right m -> return . M.match $ m

allOpts :: [MA.OptSpec (WheatConf -> WheatConf)]
allOpts =
  [ MA.OptSpec ["indentation"] "i"
    (fmap (\i p -> p { indentAmt = i }) (MA.OneArg MA.reader))

  , MA.OptSpec ["test-regexp"] "t"
    (fmap (\f p -> p { testPred = f }) (MA.OneArg parseRegexp))

  , MA.OptSpec ["stop-on-failure"] ""
    ( MA.NoArg (\p -> p { stopOnFail
                          = not (stopOnFail p) }))

  , MA.OptSpec ["color-to-file"] ""
    ( MA.NoArg (\p -> p { colorToFile
                          = not (colorToFile p) }))

  , MA.OptSpec ["base-date"] ""
    (fmap (\d p -> p { baseTime = d }) (MA.OneArg parseBaseTime))
  ]

-- | Applied to the default WheatConf, returns a new WheatConf based
-- on what was parsed from the command line, and a list of strings
-- corresponding to the ledger files provided on the command line.
parseArgs :: V.Version -> WheatConf -> IO (WheatConf, [String])
parseArgs ver c = do
  parsed <- MA.simpleHelpVersion (help c) (Ly.version ver)
            (map (fmap Right) allOpts) MA.Intersperse
            (return . Left)
  let (args, opts) = partitionEithers parsed
      fn = foldl (flip (.)) id opts
      c' = fn c
  return (c', args)


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
  (conf, args) <- parseArgs ver (getWc rt)
  term <- Rb.smartTermFromEnv (colorToFile conf) IO.stdout
  items <- Cop.open args
  let pstgs = getItems items
      formatter = formatQty conf items
  let tsts = filter ((testPred conf) . TT.testName)
             . map ($ (L.toUTC . S.currentTime $ rt))
             . tests
             $ conf
  bs <- mapM (runTest formatter conf pstgs term) tsts
  if and bs
    then Exit.exitSuccess
    else Exit.exitFailure

-- | Shows the result of a test. Exits with a failure if stopOnFail is
-- set and if the test failed. Otherwise, returns whether the test
-- succeeded or failed.
runTest
  :: (L.Amount L.Qty -> X.Text)
  -> WheatConf
  -> [L.Posting]
  -> Rb.Term
  -> TT.Test L.Posting
  -> IO Bool
runTest fmt c ps term test = do
  let rslt = TT.evalTest test ps
      cks = TT.showResult (indentAmt c) (L.display fmt)
                          (verbosity c) rslt
  Rb.putChunks term cks
  if stopOnFail c && not (TT.resultPass rslt)
    then Exit.exitFailure
    else return (TT.resultPass rslt)

getItems :: [Cop.LedgerItem] -> [L.Posting]
getItems
  = concatMap L.transactionToPostings
  . mapMaybe ( let cn = const Nothing
               in Su.caseS4 Just cn cn cn)

--
-- Tests
--

-- | Passes only if each posting is True.
eachPostingMustBeTrue
  :: TT.Name
  -> Pe.Pdct L.Posting
  -> TT.Test L.Posting
eachPostingMustBeTrue n pd = TT.eachSubjectMustBeTrue pd n

-- | Passes if at least a particular number of postings is True.
atLeastNPostings
  :: Int
  -- ^ The number of postings that must be true for the test to pass
  -> TT.Name
  -> Pe.Pdct L.Posting
  -> TT.Test L.Posting
atLeastNPostings i n pd = TT.nSubjectsMustBeTrue pd n i

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
  , "  -t, --test-regexp REGEXP"
  , "    Run only tests whose name matches the given"
  , "    Perl-compatible regular expression"
  , "    (overrides the compiled-in default)"
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

