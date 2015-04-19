{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Clatcher where

import Control.Exception (throwIO, Exception)
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
import Pipes.Cliff (pipeInput, NonPipe(..), terminateProcess, procSpec)
import Pipes.Prelude (drain)
import Pipes.Safe (register, SafeT, mask_)
import Rainbow

type PreFilter l = Matcher (TransactionL l, View (Converted (PostingL l))) l ()
type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))
type PostFilter l
  = Matcher (RunningBalance
    (Sorted (Filtered (TransactionL l, View (Converted (PostingL l)))))) l ()
type AllChunks = (Seq Chunk, Seq Chunk, Seq Chunk)

type Reporter l
  = (Amount -> RepNonNeutralNoSide)
  -> Seq (Clatch l)
  -> [Chunk]

data ClatchOptions c l ldr rpt res = ClatchOptions
  { converter :: c (Amount -> Maybe Amount)
  , preFilter :: c (PreFilter l)
  , sorter :: c (Seq (Filtereds l -> l (Filtereds l)))
  , postFilter :: c (PostFilter l)
  , convertPreFilter :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , convertPostFilter :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , convertMain :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , streamPreFilter :: c (IO (Consumer ByteString (SafeT IO) ()))
  , streamPostFilter :: c (IO (Consumer ByteString (SafeT IO) ()))
  , streamMain :: c (IO (Consumer ByteString (SafeT IO) ()))
  , report :: c (rpt, rpt -> Reporter l)
  , runLedger :: c (ldr, ldr -> l AllChunks -> IO AllChunks)
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
  = State (ClatchOptions Maybe l ldr rpt res)

-- | Loads transactions and prices from the given file on disk.
open
  :: String
  -> Clatcher Scroll (Seq Loader) rpt res ()
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
clatcher :: Clatcher l ldr rpt res a -> IO ()
clatcher = undefined

runLess :: IO (Consumer ByteString (SafeT IO) ())
runLess = mask_ $ do
  (pipe, handle) <- pipeInput Inherit Inherit
    (procSpec "less" ["-R"])
  return (register (terminateProcess handle) >> pipe >> return ())

defaultClatchOptions :: Monad l => ClatchOptions Identity l ldr rpt res
defaultClatchOptions = ClatchOptions
  { converter = Identity (const Nothing)
  , preFilter = Identity always
  , sorter = Identity Seq.empty
  , postFilter = Identity always
  , convertPreFilter = Identity byteStringMakerFromEnvironment
  , convertPostFilter = Identity byteStringMakerFromEnvironment
  , convertMain = Identity byteStringMakerFromEnvironment
  , streamPreFilter = Identity (return drain)
  , streamPostFilter = Identity (return drain)
  , streamMain = Identity runLess
  , report = Identity (undefined, \_ _ _ -> [])
  , runLedger = Identity (undefined, \_ _ -> return
      (Seq.empty, Seq.empty, Seq.empty))
  }


mergeClatchOptions
  :: ClatchOptions Identity l ldr rpt res
  -> ClatchOptions Maybe l ldr rpt res
  -> ClatchOptions Identity l ldr rpt res
mergeClatchOptions iden may = ClatchOptions
  { converter = merge converter converter
  , preFilter = merge preFilter preFilter
  , sorter = merge sorter sorter
  , postFilter = merge postFilter postFilter
  , convertPreFilter = merge convertPreFilter convertPreFilter
  , convertPostFilter = merge convertPostFilter convertPostFilter
  , convertMain = merge convertMain convertMain
  , streamPreFilter = merge streamPreFilter streamPreFilter
  , streamPostFilter = merge streamPostFilter streamPostFilter
  , streamMain = merge streamMain streamMain
  , report = merge report report
  , runLedger = merge runLedger runLedger
  }
  where
    merge fn1 fn2 = case fn1 may of
      Nothing -> fn2 iden
      Just r -> Identity r

emptyClatchOpts :: ClatchOptions Maybe l ldr rpt res
emptyClatchOpts = ClatchOptions
  { converter = Nothing
  , preFilter = Nothing
  , sorter = Nothing
  , postFilter = Nothing
  , convertPreFilter = Nothing
  , convertPostFilter = Nothing
  , convertMain = Nothing
  , streamPreFilter = Nothing
  , streamPostFilter = Nothing
  , streamMain = Nothing
  , report = Nothing
  , runLedger = Nothing
  }


clatcherWithDefaults
  :: Ledger l
  => ClatchOptions Identity l ldr rpt res
  -> Clatcher l ldr rpt res a
  -> IO ()
clatcherWithDefaults dfltOpts cltcr = do
  let opts = mergeClatchOptions dfltOpts (execState cltcr emptyClatchOpts)
      calc = allClatches (runIdentity $ converter opts)
                         (runIdentity $ preFilter opts)
                         (runIdentity $ sorter opts)
                         (runIdentity $ postFilter opts)
  undefined
