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
module Penny.Clatcher where

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
import Penny.Ledger
import Penny.Ledger.Scroll
import Penny.Matcher
import Penny.Price
import Penny.Popularity
import Penny.Representation
import Penny.SeqUtil
import Penny.Stream
import Penny.Transaction
import Rainbow

-- # Type synonyms

type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))
type AllChunks = (Seq (Chunk Text), Seq (Chunk Text), Seq (Chunk Text))

class Report a l where
  printReport
    :: Ledger l
    => a l
    -> (Amount -> NilOrBrimScalarAnyRadix)
    -> Seq (Clatch l)
    -> l (Seq (Chunk Text))

--
-- Converter
--

newtype Converter = Converter (Amount -> Maybe Amount)

makeWrapped ''Converter

instance Monoid Converter where
  mempty = Converter (const Nothing)
  mappend (Converter x) (Converter y) = Converter $ \a -> x a <|> y a

--
-- Octavo
--

data Octavo t l = Octavo
  { _filterer :: Matcher t l ()
  , _streamer :: IO Stream
  }

makeLenses ''Octavo

instance Monad l => Monoid (Octavo t l) where
  mempty = Octavo Control.Applicative.empty (return mempty)
  Octavo x0 x1 `mappend` Octavo y0 y1 = Octavo (x0 <|> y0)
    ((<>) <$> x1 <*> y1)

--
-- Errors
--

data PennyError
  = ParseError String
  deriving (Show, Typeable)

instance Exception PennyError

--
-- Loader
--

class Loader o l | o -> l where
  loadChunks :: Ledger l => l AllChunks -> o -> IO AllChunks

-- | Holds data, either loaded from a file or indicates the data to load.
data LoadScroll
  = Preloaded (Seq (Either Price (Transaction () ())))
  | OpenFile String
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

preload
  :: String
  -- ^ Load from this file
  -> IO LoadScroll
preload fn = do
  txt <- X.readFile fn
  case copperParser txt of
    Left e -> throwIO $ ParseError e
    Right sq -> return . Preloaded $ sq

openFile :: String -> LoadScroll
openFile = OpenFile


--
-- ClatchOptions
--

data ClatchOptions l r o = ClatchOptions
  { _converter :: Converter
  , _renderer :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  , _pre :: Octavo (TransactionL l, View (Converted (PostingL l))) l
  , _sorter :: Seq (Filtereds l -> l (Filtereds l))
  , _post :: Octavo (RunningBalance (Sorted (Filtered
      (TransactionL l, View (Converted (PostingL l)))))) l
  , _report :: IO Stream
  , _reporter :: r
  , _opener :: o
  }

makeLenses ''ClatchOptions

instance (Monad l, Monoid r, Monoid o) => Monoid (ClatchOptions l r o) where
  mempty = ClatchOptions
    { _converter = mempty
    , _renderer = Nothing
    , _pre = mempty
    , _sorter = mempty
    , _post = mempty
    , _report = return mempty
    , _reporter = mempty
    , _opener = mempty
    }

  mappend x y = ClatchOptions
    { _converter = _converter x <> _converter y
    , _renderer = getLast $ Last (_renderer x) <> Last (_renderer y)
    , _pre = _pre x <> _pre y
    , _sorter = _sorter x <> _sorter y
    , _post = _post x <> _post y
    , _report = (<>) <$> _report x <*> _report y
    , _reporter = _reporter x <> _reporter y
    , _opener = _opener x <> _opener y
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
  :: (Loader o l, Ledger l, Report r l)
  => ClatchOptions l (r l) o
  -> IO AllChunks
makeReport opts = loadChunks act (_opener opts)
  where
    act = do
      ((msgsPre, rndgs, msgsPost), cltchs) <-
        allClatches (opts ^. converter . _Wrapped')
                    (_filterer . _pre $ opts)
                    (_sorter opts) (_filterer . _post $ opts)
      cks <- printReport (_reporter opts)
                         (smartRender (_renderer opts) rndgs)
                         cltchs
      return (msgsToChunks msgsPre, msgsToChunks msgsPost, cks)

clatcher
  :: (Loader o l, Ledger l, Report r l)
  => ClatchOptions l (r l) o
  -> IO ()
clatcher opts = do
  (msgsPre, msgsPost, cksRpt) <- makeReport opts
  feedStream (opts ^. pre . streamer) msgsPre $
    feedStream (opts ^. post . streamer) msgsPost $
    feedStream (opts ^. report) cksRpt $
    return ()

