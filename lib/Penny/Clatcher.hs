{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Penny.Clatcher where

import Control.Lens hiding (pre)
import Control.Exception
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text.IO as X
import Penny.Clatch
import Penny.Converter
import Penny.Representation
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
  loadTransactions :: a -> IO (Seq Price, Seq Transaction)

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

instance Loader (Seq LoadScroll) where
  loadTransactions = fmap (second addSerials . combine) . traverse load
    where
      load scroll = case scroll of
        Preloaded prices txns -> return (prices, txns)
        OpenFile fn -> loadCopper fn
      combine = F.foldl' f (Seq.empty, Seq.empty)
        where
          f (pricesA, txnsA) (prices, txns)
            = (pricesA <> prices, txnsA |> txns)

data ClatchOptions r l = ClatchOptions
  { _converter :: Converter
  -- ^ Converts the amount of each posting from one amount to another.
  -- For example, this can be useful to convert a commodity to its
  -- value in your home currency.

  , _renderer :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  -- ^ Determines how to render quantities.  Original formatting is
  -- used if possible.  For some quantities that's not possible; for
  -- instance, a balance total is calculated so there is no original
  -- quantity.  In that case, the clatcher tries to use the most
  -- commonly used representation for a given commodity.  To determine
  -- this the clatcher examines all the values appearing in the set of
  -- loaded ledger files.  If that fails (perhaps the commodity was
  -- not included in the loaded ledger files), then the clatcher
  -- consults the value of '_renderer'.  If '_renderer' is 'Just',
  -- then the given radix point is used; if a particular grouping
  -- character is included, then that grouping character is used for
  -- values with absolute values greater than 9999.
  --
  -- If _renderer is 'Nothing' and all other methods to determine how
  -- to render fail, then the quantity is rendered using a period
  -- radix point and no digit grouping.

  , _pre :: Converted () -> Bool
  -- ^ Controls pre-filtering

  , _sort :: Prefilt () -> Prefilt () -> Ordering
  -- ^ Sorts postings; this is done after pre-filtering but before post-filtering.

  , _post :: Totaled () -> Bool
  -- ^ Controls post-filtering

  , _out :: IO Stream
  -- ^ The destination stream for the report.

  , _report :: r
  -- ^ What report to print

  , _load :: l
  -- ^ Source from which to load transactions and prices

  }

makeLenses ''ClatchOptions

-- | The 'Monoid' instance uses for 'mempty':
--
-- 'mempty' for '_converter'
--
-- 'Nothing' for '_renderer'
--
-- 'const' 'True' for '_pre'
--
-- 'mempty' for '_sort'
--
-- 'const' 'True' for '_post'
--
-- 'return' 'mempty' for '_out'
--
-- 'mempty' '_report'
--
-- 'mempty' '_load'
--
-- 'mappend' uses:
--
-- 'mappend' for '_converter'
--
-- the last non-Nothing value if there is one for '_renderer';
-- otherwise, Nothing
--
-- for '_pre' and '_post', returns 'True' only if both operands return 'True'
--
-- 'mappend' for '_sort'
--
-- 'mappend' both streams for '_out'
--
-- returns the results of both '_report'
--
-- returns the results of both '_load'

instance (Monoid r, Monoid l) => Monoid (ClatchOptions r l) where
  mempty = ClatchOptions
    { _converter = mempty
    , _renderer = Nothing
    , _pre = const True
    , _sort = mempty
    , _post = const True
    , _out = return mempty
    , _report = mempty
    , _load = mempty
    }

  mappend x y = ClatchOptions
    { _converter = _converter x <> _converter y
    , _renderer = getLast $ Last (_renderer x) <> Last (_renderer y)
    , _pre = \a -> _pre x a && _pre y a
    , _sort = _sort x <> _sort y
    , _post = \a -> _post x a && _post y a
    , _out = (<>) <$> (_out x) <*> (_out y)
    , _report = _report x <> _report y
    , _load = _load x <> _load y
    }

--
-- Main clatcher
--

getReport
  :: Report r
  => ClatchOptions r l
  -> (Seq Price, Seq Transaction)
  -> Seq (Chunk Text)
getReport opts items
  = printReport (opts ^. report) hist (opts ^. renderer) clatches
  where
    hist = elect . snd $ items
    clatches
      = clatchesFromTransactions (opts ^. converter) (opts ^. pre)
                                 (opts ^. sort) (opts ^. post)
                                 (snd items)

clatcher
  :: (Report r, Loader l)
  => ClatchOptions r l
  -> IO ()
clatcher opts = do
  items <- loadTransactions (opts ^. load)
  let rpt = getReport opts items
  feedStream (opts ^. out) rpt (return ())
