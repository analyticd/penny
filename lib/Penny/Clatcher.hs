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
module Penny.Clatcher
  (  -- * Converter
    Converter(..)

    -- * Octavo
  , Octavo(..)
    -- ** Lenses
  , filterer
  , streamer

    -- * Errors
  , PennyError(..)

    -- * Loader
  , Loader(..)
  , LoadScroll(..)
  , preload
  , openFile

    -- * Clatch options
  , Filtereds
  , ClatchOptions(..)
    -- ** Clatch options lenses
  , converter
  , renderer
  , pre
  , sorter
  , post
  , output
  , reporter
  , loader

  -- * Preset clatcher settings
  , presets

  -- * Running the clatcher
  , clatcher
  ) where

import Control.Applicative
import Control.Exception (throwIO, Exception)
import Control.Lens hiding (pre)
import Control.Monad.Reader
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text.IO as X
import qualified Data.Traversable as T
import Data.Typeable
import Penny.Amount
import Penny.Clatch
import Penny.Copper
import Penny.Folio (Folio)
import qualified Penny.Folio
import Penny.Ledger
import Penny.Ledger.Scroll
import Penny.Matcher
import Penny.Price
import Penny.Register
import Penny.Report
import Penny.Popularity
import Penny.Prefilt
import Penny.Representation
import Penny.SeqUtil
import Penny.Stream
import Penny.Transaction
import Rainbow


--
-- Converter
--

-- | A function that converts one 'Amount' to another.
newtype Converter = Converter (Amount -> Maybe Amount)

makeWrapped ''Converter

-- | For a given 'Amount', 'mappend' uses the first 'Converter' that
-- succeeds, or performs no conversion if neither 'Converter' performs
-- a conversion.  'mempty' performs no conversion.
instance Monoid Converter where
  mempty = Converter (const Nothing)
  mappend (Converter x) (Converter y) = Converter $ \a -> x a <|> y a

--
-- Octavo
--

-- | Every run of the clatcher produces two streams of non-report
-- text: one describing the pre-filtering process, and one describing
-- the post-filtering process.  The 'Octavo' holds two values: the
-- 'Matcher' that does the pre- or post-filtering, and the 'Stream'
-- that determines what happens to the textual description of the output.
data Octavo t l = Octavo
  { _filterer :: Matcher t l ()
  , _streamer :: IO Stream
  }

makeLenses ''Octavo

-- | 'mempty' uses a '_filterer' that returns no matches and a
-- 'Stream' that sends output to 'devNull'.  'mappend' uses '<|>' on
-- the '_filterer' and will send the output to both given
-- '_streamer's.
instance Monad l => Monoid (Octavo t l) where
  mempty = Octavo Control.Applicative.empty (return mempty)
  Octavo x0 x1 `mappend` Octavo y0 y1 = Octavo (x0 <|> y0)
    ((<>) <$> x1 <*> y1)

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
  loadChunks :: Ledger l => l Folio -> o -> IO Folio
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

-- | A sequence of filtered postings.
type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))

-- | All options necessary to run the clatcher.
data ClatchOptions l r o = ClatchOptions
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

  , _pre :: Octavo (TransactionL l, View (Converted (PostingL l))) l
  -- ^ Controls pre-filtering; see 'Octavo' for details

  , _sorter :: Seq (Filtereds l -> l (Filtereds l))
  -- ^ Sorts postings; this is done after pre-filtering but before post-filtering.
  , _post :: Octavo (RunningBalance (Sorted (Filtered
      (TransactionL l, View (Converted (PostingL l)))))) l
  -- ^ Controls post-filtering; see 'Octavo' for details
  , _output :: IO Stream
  -- ^ The destination stream for the report.
  , _reporter :: r
  -- ^ What report to print
  , _loader :: o
  -- ^ Source from which to load transactions and prices
  }

makeLenses ''ClatchOptions

-- | 'mempty' uses 'mempty' for all fields, except for '_renderer'
-- (which uses 'Nothing') and '_report' (which uses @return mempty@).
--
-- 'mappend' uses 'mappend' for all fields, except for '_renderer'
-- (which uses the last non-'Nothing' value) and '_report' (which will
-- run both streams concurrently).
instance (Monad l, Monoid r, Monoid o) => Monoid (ClatchOptions l r o) where
  mempty = ClatchOptions
    { _converter = mempty
    , _renderer = Nothing
    , _pre = mempty
    , _sorter = mempty
    , _post = mempty
    , _output = return mempty
    , _reporter = mempty
    , _loader = mempty
    }

  mappend x y = ClatchOptions
    { _converter = _converter x <> _converter y
    , _renderer = getLast $ Last (_renderer x) <> Last (_renderer y)
    , _pre = _pre x <> _pre y
    , _sorter = _sorter x <> _sorter y
    , _post = _post x <> _post y
    , _output = (<>) <$> _output x <*> _output y
    , _reporter = _reporter x <> _reporter y
    , _loader = _loader x <> _loader y
    }

--
-- Messages
--

msgsToChunks
  :: Seq (Seq Message)
  -> Seq (Chunk Text)
msgsToChunks = join . join . fmap (fmap (Seq.fromList . ($ []) . toChunks))

--
-- Main clatcher
--

smartRender
  :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  -> Renderings
  -> Amount
  -> NilOrBrimScalarAnyRadix
smartRender mayRndrer (Renderings rndgs) (Amount cy qt)
  = c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  $ repQtySmartly rndrer (fmap (fmap snd) rndgs) cy qt
  where
    rndrer = maybe (Right Nothing) id mayRndrer

makeReport
  :: (Loader o l, Ledger l, Report r)
  => ClatchOptions l (r l) o
  -> IO Folio
makeReport opts = loadChunks act (_loader opts)
  where
    act = do
      ((msgsPre, rndgs, msgsPost), cltchs) <-
        allClatches (opts ^. converter . _Wrapped')
                    (_filterer . _pre $ opts)
                    (_sorter opts) (_filterer . _post $ opts)
      cks <- printReport (_reporter opts)
                         (smartRender (_renderer opts) rndgs)
                         cltchs
      return $ Penny.Folio.Folio
        { Penny.Folio._pre = msgsToChunks msgsPre
        , Penny.Folio._post = msgsToChunks msgsPost
        , Penny.Folio._report = cks
        }

clatcher
  :: (Loader o l, Ledger l, Report r)
  => ClatchOptions l (r l) o
  -> IO ()
clatcher opts = do
  folio <- makeReport opts
  feedStream (opts ^. pre . streamer) (folio ^. Penny.Folio.pre) $
    feedStream (opts ^. post . streamer) (folio ^. Penny.Folio.post) $
    feedStream (opts ^. output) (folio ^. Penny.Folio.report) $
    return ()

-- | A reasonable set of defaults for 'ClatchOptions'.  You could use
-- 'mempty', but that results in a 'ClatchOptions' that will do
-- nothing.
--
-- Defaults:
--
-- * If automatic rendering fails, the radix point is a period and
-- grouping is performed with commas
--
-- * The pre-filter accepts all postings
--
-- * The post-filter accepts all postings
--
-- * Report output is sent to @less@
--
-- * The 'register' report is used
--
-- * Column headings are not shown
--
-- * Colors for a light background are used

presets
  :: (Monoid o, Ledger l)
  => ClatchOptions l (Register l) o
presets = mempty
  & renderer .~ Just (Right . Just $ Comma)
  & pre.filterer .~ always
  & post.filterer .~ always
  & output .~ toStream toLess
  & reporter.columns .~ register
  & reporter.colors .~ lightBackground
