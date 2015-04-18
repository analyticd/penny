{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Clatcher where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bifunctor.Joker
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
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
import Penny.Representation
import Penny.SeqUtil
import Penny.Transaction
import Pipes
import Rainbow

type PreFilter l = Matcher (TransactionL l, View (Converted (PostingL l))) l ()
type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))
type PostFilter l
  = Matcher (RunningBalance
    (Sorted (Filtered (TransactionL l, View (Converted (PostingL l)))))) l ()

type Reporter l
  = (Amount -> RepNonNeutralNoSide)
  -> Seq (Clatch l)
  -> [Chunk]

data ClatchOptions l ldr rpt res = ClatchOptions
  { converter :: Maybe (Amount -> Maybe Amount)
  , preFilter :: Maybe (PreFilter l)
  , sorter :: Maybe (Filtereds l -> l (Filtereds l))
  , postFilter :: Maybe (PostFilter l)
  , streamPreFilter :: Maybe (Consumer ByteString IO ())
  , streamPostFilter :: Maybe (Consumer ByteString IO ())
  , streamMain :: Maybe (Consumer ByteString IO ())
  , report :: Maybe (rpt, rpt -> Reporter l)
  , runLedger :: Maybe (ldr, ldr -> l res -> IO res)
  }

data Loader
  = LoadFromFile String
  -- ^ Gets transactions from the given filename
  | Preloaded (Seq (Either Price (Transaction () ())))
  -- ^ Uses the given transactions which have already been loaded

data PennyError
  = ParseError String
  deriving (Show, Typeable)

instance Exception PennyError

loadAndParse
  :: String
  -- ^ Filename
  -> IO (Seq (Either Price (Transaction () ())))
loadAndParse fn = do
  txt <- X.readFile fn
  case copperParser txt of
    Left e -> throwIO $ ParseError e
    Right g -> return g

loadFromLoader :: Loader -> IO (Seq (Either Price (Transaction () ())))
loadFromLoader (LoadFromFile str) = loadAndParse str
loadFromLoader (Preloaded sq) = return sq

stripUnits
  :: Seq (Seq (Either a (Transaction ((), b) ((), c))))
  -> Seq (Seq (Either a (Transaction b c)))
stripUnits = fmap (fmap (second (first snd . second snd)))

addSerials
  :: Seq (Seq (Either a (Transaction () ())))
  -> Seq (Seq (Either a (Transaction ((), TopLineSer) ((), PostingSer))))
addSerials
  = fmap (fmap runJoker)
  . fmap getCompose
  . assignSerialsToTxns
  . fmap Compose
  . fmap (fmap Joker)

loadLedger
  :: Seq Loader
  -> Scroll a
  -> IO a
loadLedger loaders (ScrollT rdr) = do
  seqs <- T.mapM loadFromLoader loaders
  let env = stripUnits . addSerials $ seqs
  return . runIdentity $ runReaderT rdr env

type Clatcher l ldr rpt res
  = State (ClatchOptions l ldr rpt res)

data ClatchDefaults = ClatchDefaults

-- | Loads transactions and prices from the given file on disk.
open :: String -> Clatcher Scroll (Seq Loader) rpt res ()
open str = modify f
  where
    f x = x { runLedger = Just runLedger' }
      where
        runLedger' = case runLedger x of
          Nothing -> (Seq.singleton (LoadFromFile str), loadLedger)
          Just (rnr, _) -> (rnr |> LoadFromFile str, loadLedger)

-- | Uses a given set of transactions and prices.  Useful if you have
-- a file that takes a very long time to load, and you want to use it
-- multiple times: you load the file yourself, and then supply the
-- data here.
reuse
  :: Seq (Either Price (Transaction () ()))
  -> Clatcher Scroll (Seq Loader) rpt res ()
reuse preload = modify f
  where
    f x = x { runLedger = Just runLedger' }
      where
        runLedger' = case runLedger x of
          Nothing -> (Seq.singleton (Preloaded preload), loadLedger)
          Just (rnr, _) -> (rnr |> Preloaded preload, loadLedger)


-- | Runs the given command with options.
clatcher :: ClatchOptions l ldr rpt res -> IO ()
clatcher = undefined
