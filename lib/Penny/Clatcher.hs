{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Penny.Clatcher where

import Control.Lens hiding (pre)
import Control.Exception
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text.IO as X
import Penny.Clatch
import Penny.Colors
import Penny.Converter
import Penny.Copper
import Penny.Ents
import Penny.Popularity
import Penny.Price
import Penny.Report
import Penny.SeqUtil
import Penny.Stream
import Penny.Tree
import Data.Monoid
import Data.Typeable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Rainbow

-- | Describes any errors that may arise in the clatcher.
data PennyError
  = ParseError String
  -- ^ A file failed to parse.
  deriving (Show, Typeable)

instance Exception PennyError

class Loader a where
  loadTransactions :: Seq a -> IO (Seq Price, Seq Transaction)

data LoadScroll
  = Preloaded (Seq Price) (Seq (Seq Tree, Balanced (Seq Tree)))
  | OpenFile String

loadCopper :: String -> IO (Seq Price, Seq (Seq Tree, Balanced (Seq Tree)))
loadCopper fn = do
  txt <- X.readFile fn
  case copperParser txt of
    Left err -> Control.Exception.throw . ParseError $ err
    Right g -> return (prices, txns)
      where
        (prices, txns) = partitionEithers g

preload :: String -> IO LoadScroll
preload = fmap (\(prices, txns) -> Preloaded prices txns) . loadCopper

open :: String -> LoadScroll
open = OpenFile

instance Loader LoadScroll where
  loadTransactions = fmap (second addSerials . combine) . traverse load
    where
      load scroll = case scroll of
        Preloaded prices txns -> return (prices, txns)
        OpenFile fn -> loadCopper fn
      combine = F.foldl' f (Seq.empty, Seq.empty)
        where
          f (pricesA, txnsA) (prices, txns)
            = (pricesA <> prices, txnsA |> txns)

data Clatcher r l = Clatcher
  { _converter :: Converter
  -- ^ Converts the amount of each posting from one amount to another.
  -- For example, this can be useful to convert a commodity to its
  -- value in your home currency.

  , _sieve :: Converted () -> Bool
  -- ^ Controls pre-filtering

  , _sort :: Prefilt () -> Prefilt () -> Ordering
  -- ^ Sorts postings; this is done after pre-filtering but before post-filtering.

  , _screen :: Totaled () -> Bool
  -- ^ Controls post-filtering

  , _output :: IO Stream
  -- ^ The destination stream for the report.

  , _colors :: Colors
  -- ^ What colors to use for reports

  , _report :: Seq r
  -- ^ What report to print

  , _load :: Seq l
  -- ^ Source from which to load transactions and prices

  }

makeLenses ''Clatcher


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
-- 'return' 'mempty' for '_out'
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
-- 'mappend' both streams for '_output'
--
-- 'mappend' for '_colors'
--
-- returns the results of both '_report'
--
-- returns the results of both '_load'

instance Monoid (Clatcher r l) where
  mempty = Clatcher
    { _converter = mempty
    , _sieve = const True
    , _sort = mempty
    , _screen = const True
    , _output = return mempty
    , _colors = mempty
    , _report = mempty
    , _load = mempty
    }

  mappend x y = Clatcher
    { _converter = _converter x <> _converter y
    , _sieve = \a -> view sieve x a && view sieve y a
    , _sort = _sort x <> _sort y
    , _screen = \a -> view screen x a && view screen y a
    , _output = (<>) <$> (_output x) <*> (_output y)
    , _colors = _colors x <> _colors y
    , _report = _report x <> _report y
    , _load = _load x <> _load y
    }

--
-- Main clatcher
--

getReport
  :: Report r
  => Clatcher r l
  -> (Seq Price, Seq Transaction)
  -> Seq (Chunk Text)
getReport opts items
  = printReport (opts ^. report) (opts ^. colors) hist clatches
  where
    hist = elect . snd $ items
    clatches
      = clatchesFromTransactions (opts ^. converter) (view sieve opts)
                                 (opts ^. sort) (view screen opts)
                                 (snd items)

clatcher
  :: (Report r, Loader l)
  => Clatcher r l
  -> IO ()
clatcher opts = do
  items <- loadTransactions (opts ^. load)
  let rpt = getReport opts items
  feedStream (opts ^. output) rpt (return ())

