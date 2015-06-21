{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | The clatcher has a user interface intended to be used in the GHCi
-- REPL.  It loads transactions and prices from mulitple files and
-- then places them into multiple 'Clatch'.  The resulting postings
-- are filtered in multiple ways, sorted, and sent to a report.
module Penny.Clatcher where

import Control.Exception (throwIO, Exception)
import Control.Lens hiding (pre)
import Control.Monad.Reader
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.IO as X
import qualified Data.Traversable as T
import Data.Typeable
import Penny.Amount
import Penny.Clatch
import Penny.Copper
import Penny.Ledger
import Penny.Ledger.Scroll
import Penny.Price
import Penny.Report
import Penny.Popularity
import Penny.Representation
import Penny.SeqUtil
import Penny.Stream
import Penny.Transaction
import Penny.Transbox
import Penny.Viewpost
import Penny.Converted
import Penny.Filtered
import Penny.Sorted
import Penny.Balance
import Rainbow


--
-- Errors
--

-- | Describes any errors that may arise in the clatcher.
data PennyError
  = ParseError String
  -- ^ A file failed to parse.
  deriving (Show, Typeable)

instance Exception PennyError

--
-- Loader
--

-- | Things that can load transactions and prices.  Generally the
-- items will come from a disk file, though they could come from
-- elsewhere.  @o@ is the value that does the loading, while @l@ is
-- the type of the ledger; each @o@ can be associated with only one
-- @l@.  For an example instance see 'LoadScroll'.
class Loader o l | o -> l where
  loadChunks :: Ledger l => l (Chunk Text) -> o -> IO (Chunk Text)
  -- ^ Given a 'Ledger' computation that makes a 'Folio' and a given
  -- value, load the necessary prices and transactions to create the
  -- 'Folio'.

-- | Holds data, either loaded from a file or indicates the data to load.
data LoadScroll
  = Preloaded (Seq (Either Price (Transaction () ())))
  -- ^ Useful to load data into memory and use it with more than one
  -- run of the clatcher.
  | OpenFile String
  -- ^ Load data from the given filename.
  deriving (Eq, Show, Ord)

readAndParseScroll
  :: LoadScroll
  -> IO (Seq (Either Price (Transaction () ())))
readAndParseScroll (Preloaded sq) = return sq
readAndParseScroll (OpenFile fn)
  = (either (throwIO . ParseError) return . copperParser)
  <=< X.readFile
  $ fn

instance Loader (Seq LoadScroll) Scroll where
  loadChunks (ScrollT act)
    = liftM (runReader act . addSerials)
    . T.mapM readAndParseScroll

-- | Preload prices and transactions from the given file and keep them
-- around for later use.
preload
  :: String
  -- ^ Load from this file
  -> IO LoadScroll
preload fn = do
  txt <- X.readFile fn
  case copperParser txt of
    Left e -> throwIO $ ParseError e
    Right sq -> return . Preloaded $ sq

-- | Load data from the given filename; has to reload the data for
-- each run of the clatcher.
openFile :: String -> LoadScroll
openFile = OpenFile


--
-- ClatchOptions
--

-- | All options necessary to run the clatcher.
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

  , _pre :: Transbox l (Viewpost l (Converted ())) -> l Bool
  -- ^ Controls pre-filtering

  , _sorter
      :: Sorter l (Transbox l (Viewpost l (Converted (Filtered ()))))
  -- ^ Sorts postings; this is done after pre-filtering but before post-filtering.

  , _post
      :: Transbox l (Viewpost l (Converted (Filtered
                                 (Sorted (RunningBalance ())))))
      -> l Bool
  -- ^ Controls post-filtering
  , _output :: IO Stream
  -- ^ The destination stream for the report.
  , _reporter :: r
  -- ^ What report to print
  , _loader :: Seq LoadScroll
  -- ^ Source from which to load transactions and prices
  }

makeLenses ''ClatchOptions

-- | The 'Monoid' instance uses for 'mempty':
--
-- 'mempty' for '_converter'
--
-- 'Nothing' for '_renderer'
--
-- 'return' 'True' for '_pre'
--
-- 'mempty' for '_sorter'
--
-- 'return' 'True' for '_post'
--
-- 'return' 'mempty' for '_output'
--
-- returns an empty 'Seq' for '_reporter'
--
-- returns an empty 'Seq' for '_loader'
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
-- 'mappend' for '_sorter'
--
-- 'mappend' both streams for '_output'
--
-- returns the results of both '_reporter'
--
-- returns the results of both '_loader'

instance (Monad l, Monoid r) => Monoid (ClatchOptions r l) where
  mempty = ClatchOptions
    { _converter = mempty
    , _renderer = Nothing
    , _pre = const $ return True
    , _sorter = mempty
    , _post = const $ return True
    , _output = return mempty
    , _reporter = mempty
    , _loader = mempty
    }

  mappend x y = ClatchOptions
    { _converter = _converter x <> _converter y
    , _renderer = getLast $ Last (_renderer x) <> Last (_renderer y)
    , _pre = \b -> liftM2 (&&) (_pre x b) (_pre y b)
    , _sorter = _sorter x <> _sorter y
    , _post = \b -> liftM2 (&&) (_post x b) (_post y b)
    , _output = liftM2 (<>) (_output x) (_output y)
    , _reporter = _reporter x <> _reporter y
    , _loader = _loader x <> _loader y
    }


--
-- Main clatcher
--

smartRender
  :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  -> Abridged
  -> Amount
  -> NilOrBrimScalarAnyRadix
smartRender mayRndrer abridged amt
  = c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  $ repQtySmartly rndrer abridged amt
  where
    rndrer = maybe (Right Nothing) id mayRndrer

clatcher
  :: Report r
  => ClatchOptions (r Scroll) Scroll
  -> IO ()
clatcher opts = do
  env <- loadTransactions (opts ^. loader)
  let ScrollT rdr = getReport opts
      chunks = runIdentity $ runReaderT rdr env
  feedStream (opts ^. output) chunks (return ())

getReport
  :: (Ledger l, Report r)
  => ClatchOptions (r l) l
  -> l (Seq (Chunk Text))
getReport opts = do
  abridged <- fmap abridge elect
  let rend = smartRender (opts ^. renderer) abridged
  txns <- fmap Penny.SeqUtil.rights . fmap join $ vault
  cltchs <- clatches (opts ^. converter)
                     (opts ^. pre)
                     (opts ^. sorter)
                     (opts ^. post)
                     txns
  printReport (opts ^. reporter) rend cltchs

loadTransactions
  :: Seq LoadScroll
  -> IO (Seq (Seq (Either Price (Transaction TopLineSer PostingSer))))
loadTransactions = fmap assignSerialsToEithers . traverse readAndParseScroll
