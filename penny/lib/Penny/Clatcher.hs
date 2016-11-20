{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- | Creates clatches and prints reports.
module Penny.Clatcher where

import Control.Exception (Exception)
import qualified Control.Lens as Lens
import Control.Monad (join)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Rainbow (Chunk)

import Penny.Clatch.Types
import Penny.Clatch.Create
import Penny.Colorize
import Penny.Colors
import Penny.Converter
import Penny.Cursor
import Penny.Popularity
import Penny.Price
import Penny.Stream
import Penny.Transaction

-- | Describes any errors that may arise in the clatcher.
data PennyError
  = ParseError String
  -- ^ A file failed to parse.
  deriving (Show, Typeable)

instance Exception PennyError

type Loader
  = IO (Seq (Price (Maybe Cursor)), Seq (Transaction (Maybe Cursor)))

type Report
  = Seq (Price (Maybe Cursor))
  -> Colors
  -> History
  -> Seq (Clatch (Maybe Cursor))
  -> Seq (Chunk Text)

data Clatcher = Clatcher
  { _converter :: Converter
  -- ^ Converts the amount of each posting from one amount to another.
  -- For example, this can be useful to convert a commodity to its
  -- value in your home currency.

  , _sieve :: Converted (Maybe Cursor) () -> Bool
  -- ^ Controls pre-filtering

  , _sort :: Prefilt (Maybe Cursor) () -> Prefilt (Maybe Cursor) () -> Ordering
  -- ^ Sorts postings; this is done after pre-filtering but before post-filtering.

  , _screen :: Totaled (Maybe Cursor) () -> Bool
  -- ^ Controls post-filtering

  , _output :: Seq (ChooseColors, Stream)
  -- ^ The destination stream for the report.

  , _colors :: Colors
  -- ^ What colors to use for reports

  , _report :: Report
  -- ^ What report to print

  , _load :: Seq Loader
  -- ^ Source from which to load transactions and prices

  }

-- | Converts the amount of each posting from one amount to another.
-- For example, this can be useful to convert a commodity to its
-- value in your home currency.
converter :: Lens.Lens' Clatcher Converter
converter = Lens.lens _converter (\b l -> b { _converter = l })

-- | The 'sieve' performs pre-filtering.  That is, it filters
-- 'Converted' after they have been converted using 'convert', but
-- before they have been sorted.  This can be useful for selecting
-- only the postings you want to be included in the running balance.
-- For instance, if you are interested in the running balance in your
-- checking account, you would pass an appropriate filter for the
-- 'sieve'.
--
-- When combining multiple 'Clatcher' using '<>', every 'sieve' must
-- be 'True' for a 'Converted' to be included in the running balance
-- and report.  Therefore, if you want to combine different 'sieve'
-- predicates using '||', you must do so (using '|||' if you wish) and
-- pass the resulting single predicate to 'sieve'.
-- Lens.makeLensesWith (Lens.set Lens.generateSignatures False Lens.lensRules)
--  ''Clatcher
sieve :: Lens.Lens' Clatcher (Converted (Maybe Cursor) () -> Bool)
sieve = Lens.lens _sieve (\b l -> b { _sieve = l })

-- | Sorts the 'Prefilt'.  This is done after pre-filtering but before
-- post-filtering.  If combining multiple 'Clatcher' using '<>', an
-- appropriate mulitple-key sort is performed.
sort
  :: Lens.Lens' Clatcher
     (Prefilt (Maybe Cursor) () -> Prefilt (Maybe Cursor) () -> Ordering)
sort = Lens.lens _sort (\b l -> b { _sort = l })

-- | Controls post-filtering.  This filtering is performed after
-- sorting and after the running balance is added.  So for example,
-- you might use 'sieve' to filter for postings that are from your
-- checking account so that the running balance includes all checking
-- postings.  In such a case, you would probably also want to use
-- 'sort' to make sure the postings are in chronological order before
-- the running balance is computed.  Then, you can use 'screen' to
-- only see particular postings you are interested in, such as
-- postings after a certain date or postings to a particular payee.
--
-- As with 'sieve', when combining multiple 'Clatcher' using '<>',
-- every 'screen' must be 'True' for a 'Totaled' to be included in the
-- report.  Therefore, if you want to combine different 'screen'
-- predicates using '||', you must do so (using '|||' if you wish) and
-- pass the resulting single predicate to 'screen'.
screen :: Lens.Lens' Clatcher (Totaled (Maybe Cursor) () -> Bool)
screen = Lens.lens _screen (\b l -> b { _screen = l })


-- | Determines where your output goes.  When combining multiple
-- 'Clatcher', every 'output' will be used.
output :: Lens.Lens' Clatcher (Seq (ChooseColors, Stream))
output = Lens.lens _output (\b l -> b { _output = l })

-- | Choose a color scheme.
colors :: Lens.Lens' Clatcher Colors
colors = Lens.lens _colors (\b l -> b { _colors = l })

-- | Choose which report to run.  If you use multiple 'report'
-- options, the reports will be shown one after the other.
report :: Lens.Lens' Clatcher Report
report = Lens.lens _report (\b l -> b { _report = l })

-- | Sources from which to load transactions and prices.
load :: Lens.Lens' Clatcher (Seq Loader)
load = Lens.lens _load (\b l -> b { _load = l })

-- | The 'Monoid' instance uses for 'mempty':
--
-- 'mempty' for '_converter'
--
-- 'const' 'True' for '_sieve'
--
-- 'mempty' for '_sort'
--
-- 'const' 'True' for '_screen'
--
-- 'mempty' for '_output'
--
-- 'mempty' for '_colors'
--
-- 'mempty' for '_chooseColors'
--
-- 'mempty' '_report'
--
-- 'mempty' '_load'
--
-- 'mappend' uses:
--
-- 'mappend' for '_converter'
--
-- for '_sieve' and '_screen', returns 'True' only if both operands return 'True'
--
-- 'mappend' for '_sort'
--
-- 'mappend' for '_output'
--
-- 'mappend' for '_colors'
--
-- 'mappend' for '_chooseColors'
--
-- returns the results of both '_report'
--
-- returns the results of both '_load'

instance Monoid Clatcher where
  mempty = Clatcher
    { _converter = mempty
    , _sieve = const True
    , _sort = mempty
    , _screen = const True
    , _output = mempty
    , _colors = mempty
    , _report = \_ _ _ _ -> mempty
    , _load = mempty
    }

  mappend x y = Clatcher
    { _converter = mappend (_converter x) (_converter y)
    , _sieve = \c -> _sieve x c && _sieve y c
    , _sort = mappend (_sort x) (_sort y)
    , _screen = \c -> _screen x c && _screen y c
    , _output = mappend (_output x) (_output y)
    , _colors = mappend (_colors x) (_colors y)
    , _report = \a b c d -> mappend (_report x a b c d) (_report y a b c d)
    , _load = mappend (_load x) (_load y)
    }

-- | Sends output to a 'Stream'.
sendToStream
  :: Seq (Chunk Text)
  -> ChooseColors
  -> Stream
  -> IO ()
sendToStream chunks cc str = do
  converter <- getColorizer cc
  str converter chunks

-- | Runs the clatcher, sending output to the streams specified in
-- '_output'.  Also, returns the report.
runClatcher
  :: Clatcher
  -> IO (Seq (Chunk Text))
runClatcher clatcher = do
  priceTxnPairs <- sequence (_load clatcher)
  let prices = join . fmap fst $ priceTxnPairs
  let pennyTxns = fmap snd priceTxnPairs
  let clatchTxns = addSerials pennyTxns
  let clatches = clatchesFromTransactions (_converter clatcher)
        (_sieve clatcher) (_sort clatcher) (_screen clatcher) clatchTxns
  let history = elect clatchTxns
  let chunks = _report clatcher prices (_colors clatcher) history
        clatches
  mapM_ (uncurry (sendToStream chunks)) . _output $ clatcher
  return chunks
