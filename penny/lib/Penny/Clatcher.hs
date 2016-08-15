{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- | Creates clatches and prints reports.
module Penny.Clatcher where

import Control.Exception (Exception, throwIO)
import qualified Control.Lens as Lens
import Control.Monad (join)
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as XIO
import Data.Typeable (Typeable)
import Rainbow (Chunk)

import Penny.Clatch.Types
import Penny.Clatch.Create
import Penny.Colors
import Penny.Converter
import qualified Penny.Copper as Copper
import Penny.Copper.Tracompri
import Penny.Cursor
import Penny.Popularity
import Penny.Price
import Penny.SeqUtil
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

loadCopper :: String -> Loader
loadCopper fn = do
  txt <- XIO.readFile fn
  case Copper.parseConvertProof (X.pack fn, txt) of
    Left err -> throwIO err
    Right (_, tras) -> return (prices, txns)
      where
        (prices, txns) = partitionEithers . catMaybes
          . fmap toEi . fmap (fmap Just) $ tras
        toEi tra = case tra of
          Tracompri'Transaction x -> Just (Right x)
          Tracompri'Price x -> Just (Left x)
          _ -> Nothing

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

  , _output :: Seq Stream
  -- ^ The destination stream for the report.

  , _colors :: Colors
  -- ^ What colors to use for reports

  , _report :: Report
  -- ^ What report to print

  , _load :: Seq Loader
  -- ^ Source from which to load transactions and prices

  }

Lens.makeLenses ''Clatcher

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
  runStreams chunks (_output clatcher)
  return chunks
