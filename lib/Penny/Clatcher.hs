{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Clatcher where

import qualified Control.Concurrent.Async as Async
import Control.Exception (throwIO, Exception, bracketOnError)
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
import Data.Void
import Penny.Amount
import Penny.Clatch
import Penny.Copper
import Penny.Ledger
import Penny.Ledger.Scroll
import Penny.Matcher
import Penny.Price
import Penny.Qty
import Penny.Representation
import Penny.SeqUtil
import Penny.Transaction
import Pipes
import Pipes.Cliff (pipeInput, NonPipe(..), terminateProcess, procSpec
  , waitForProcess)
import Pipes.Prelude (drain)
import Pipes.Safe (SafeT, runSafeT)
import Rainbow

-- # Type synonyms

type PreFilter l = Matcher (TransactionL l, View (Converted (PostingL l))) l ()
type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))
type PostFilter l
  = Matcher (RunningBalance
    (Sorted (Filtered (TransactionL l, View (Converted (PostingL l)))))) l ()
type AllChunks = (Seq Chunk, Seq Chunk, Seq Chunk)

type Reporter l
  = (Amount -> NilOrBrimScalarAnyRadix)
  -> Seq (Clatch l)
  -> [Chunk]

-- # Serials

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


-- # ClatchOptions

data Octavo c a = Octavo
  { filterer :: c a
  , streamer :: c (IO Stream)
  , colorizer :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  }


data ClatchOptions c l ldr rpt = ClatchOptions
  { converter :: c (Amount -> Maybe Amount)
  , renderer :: c (Either (Maybe RadCom) (Maybe RadPer))
  , preFilter :: Octavo c (PreFilter l)
  , sorter :: c (Seq (Filtereds l -> l (Filtereds l)))
  , postFilter :: Octavo c (PostFilter l)
  , report :: Octavo c Void
  , reportData :: c (rpt, rpt -> Reporter l)
  , runLedger :: c (ldr, ldr -> l AllChunks -> IO AllChunks)
  }

type Clatcher l ldr rpt
  = State (ClatchOptions Maybe l ldr rpt)

defaultClatchOptions :: Monad l => ClatchOptions Identity l ldr rpt
defaultClatchOptions = ClatchOptions
  { converter = Identity (const Nothing)
  , renderer = Identity (Right (Just Comma))
  , preFilter = Octavo (Identity always) (Identity (return devNull))
      (Identity byteStringMakerFromEnvironment)
  , sorter = Identity Seq.empty
  , postFilter = Octavo (Identity always) (Identity (return devNull))
      (Identity byteStringMakerFromEnvironment)
  , report = Octavo (Identity undefined) (Identity runLess)
      (Identity byteStringMakerFromEnvironment)
  , reportData = Identity (undefined, \_ _ _ -> [])
  , runLedger = Identity (undefined, \_ _ -> return
      (Seq.empty, Seq.empty, Seq.empty))
  }


mergeClatchOptions
  :: ClatchOptions Identity l ldr rpt
  -> ClatchOptions Maybe l ldr rpt
  -> ClatchOptions Identity l ldr rpt
mergeClatchOptions iden may = ClatchOptions
  { converter = merge converter converter
  , renderer = merge renderer renderer
  , preFilter = mergeO preFilter preFilter
  , sorter = merge sorter sorter
  , postFilter = mergeO postFilter postFilter
  , report = mergeO report report
  , reportData = merge reportData reportData
  , runLedger = merge runLedger runLedger
  }
  where
    mergeO f1 f2 = mergeOctavo (f1 iden) (f2 may)
    merge fn1 fn2 = case fn1 may of
      Nothing -> fn2 iden
      Just r -> Identity r

mergeOctavo
  :: Octavo Identity a
  -> Octavo Maybe a
  -> Octavo Identity a
mergeOctavo l r = Octavo (merge filterer filterer) (merge streamer streamer)
  (merge colorizer colorizer)
  where
    merge getL getR = case getR r of
      Nothing -> getL l
      Just a -> Identity a

emptyClatchOpts :: ClatchOptions Maybe l ldr rpt
emptyClatchOpts = ClatchOptions
  { converter = Nothing
  , renderer = Nothing
  , preFilter = emptyOctavo
  , sorter = Nothing
  , postFilter = emptyOctavo
  , report = emptyOctavo
  , reportData = Nothing
  , runLedger = Nothing
  }

emptyOctavo :: Octavo Maybe a
emptyOctavo = Octavo Nothing Nothing Nothing


-- # Streams

-- | An action that waits on a particular stream to finish.  This
-- action should block until the stream is done.
newtype Waiter = Waiter (IO ())

-- | An action that terminates a stream right away.
newtype Terminator = Terminator (IO ())

-- | A stream that accepts 'ByteString', coupled with an action that
-- terminates the stream right away and an action that waits for the
-- stream to terminate normally.
data Stream = Stream (Consumer ByteString (SafeT IO) ()) Waiter Terminator

streamConsumer :: Stream -> Consumer ByteString (SafeT IO) ()
streamConsumer (Stream s _ _) = s

terminate :: Stream -> IO ()
terminate (Stream _ _ (Terminator t)) = t

wait :: Stream -> IO ()
wait (Stream _ (Waiter w) _) = w

devNull :: Stream
devNull = Stream drain (Waiter (return ())) (Terminator (return ()))

-- | Runs a stream that accepts on its standard input.
streamToStdin
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Arguments
  -> IO Stream
streamToStdin name args = do
  (pipe, handle) <- pipeInput Inherit Inherit (procSpec name args)
  return (Stream (pipe >> return ())
                 (Waiter (waitForProcess handle >> return ()))
                 (Terminator (terminateProcess handle)))

-- | Runs @less@, with an option to recognize ANSI color codes.
runLess :: IO Stream
runLess = streamToStdin "less" ["-R"]

-- | Runs a stream.  Under normal circumstances, waits for the
-- underlying process to stop running.  If an exception is thrown,
-- terminates the process immediately.
withStream
  :: IO Stream
  -> (Consumer ByteString (SafeT IO) () -> IO b)
  -> IO b
withStream acq use = bracketOnError acq terminate $ \str -> do
  r <- use (streamConsumer str)
  wait str
  return r

-- # Feeding streams

chunkConverter
  :: Monad m
  => (Chunk -> [ByteString] -> [ByteString])
  -> Pipe Chunk ByteString m a
chunkConverter f = do
  ck <- await
  let bss = f ck []
  mapM_ yield bss
  chunkConverter f

feedStream
  :: IO Stream
  -> (Chunk -> [ByteString] -> [ByteString])
  -> Seq Chunk
  -> IO b
  -> IO b
feedStream strm conv sq rest = withStream strm $ \str -> do
  let act = runSafeT $ runEffect $ Pipes.each sq >-> chunkConverter conv
            >-> str
  bracketOnError (Async.async act) Async.cancel $ \asy -> do
    a <- rest
    Async.wait asy
    return a

feeder
  :: Octavo Identity a
  -> Seq Chunk
  -> IO b
  -> IO b
feeder oct sq rest = do
  let strm = streamer oct
  cvtr <- runIdentity $ colorizer oct
  feedStream (runIdentity strm) cvtr sq rest


-- # Errors

data PennyError
  = ParseError String
  deriving (Show, Typeable)

instance Exception PennyError


-- # Main clatcher

makeReport
  :: Ledger l
  => ClatchOptions Identity l ldr rpt
  -> IO AllChunks
makeReport opts = getChunks ldr ledgerCalc
  where
    (ldr, getChunks) = runIdentity $ runLedger opts
    ledgerCalc = do
      ((msgsPre, Renderings rndgs, msgsPost), cltchs) <-
        allClatches (runIdentity $ converter opts)
                    (runIdentity . filterer $ preFilter opts)
                    (runIdentity $ sorter opts)
                    (runIdentity . filterer $ postFilter opts)
      let smartRender (Amount cy qt)
            = c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
            $ repQtySmartly (runIdentity $ renderer opts)
                            (fmap (fmap snd) rndgs) cy qt
          (rptOpts, mkReport) = runIdentity $ reportData opts
          cks = mkReport rptOpts smartRender cltchs
      return (msgsToChunks msgsPre, msgsToChunks msgsPost, Seq.fromList cks)

msgsToChunks
  :: Seq (Seq Message)
  -> Seq Chunk
msgsToChunks = join . join . fmap (fmap (Seq.fromList . ($ []) . toChunks))


-- | Runs the given 'Clatcher' with the given default options.
clatcherWithDefaults
  :: Ledger l
  => ClatchOptions Identity l ldr rpt
  -- ^ Default options used if there was not one selected in the 'Clatcher'.
  -> Clatcher l ldr rpt a
  -- ^ Options
  -> IO ()
clatcherWithDefaults dfltOpts cltcr = do
  let opts = mergeClatchOptions dfltOpts (execState cltcr emptyClatchOpts)
  (msgsPre, msgsPost, cksRpt) <- makeReport opts
  feeder (preFilter opts) msgsPre $
    feeder (postFilter opts) msgsPost $
    feeder (report opts) cksRpt $
    return ()


-- | Runs the given command with options.
clatcher :: Ledger l => Clatcher l ldr rpt a -> IO ()
clatcher = clatcherWithDefaults defaultClatchOptions


-- # Available options

-- ## Conversion

-- | Adds a converter.  This allows you to convert one commodity to
-- another; for example, this allows you to see what a commodity is
-- worth in terms of your home currency.
--
-- If there was already a converter present, it will be applied first,
-- and this converter will be applied only if the first fails.

convert
  :: (Amount -> Maybe Amount)
  -> Clatcher l ldr rpt ()
convert conv = modify f
  where
    f o = o { converter = conv' }
      where
        conv' = case converter o of
          Nothing -> Just conv
          Just oldConv -> Just $ \a -> case oldConv a of
            Nothing -> conv a
            Just a' -> Just a'

-- | Sets the default radix point and grouping character for
-- rendering.  This is used only if a radix point cannot be determined
-- from existing transactions.
radGroup
  :: Either (Maybe RadCom) (Maybe RadPer)
  -> Clatcher l ldr rpt ()
radGroup ei = modify f
  where
    f o = o { renderer = r' }
      where
        r' = Just . maybe ei (const ei) . renderer $ o

-- ## Dealing with files

data Loader
  = LoadFromFile String
  -- ^ Gets transactions from the given filename
  | Preloaded (Seq (Either Price (Transaction () ())))
  -- ^ Uses the given transactions which have already been loaded

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

loadLedger
  :: Seq Loader
  -> Scroll a
  -> IO a
loadLedger loaders (ScrollT rdr) = do
  seqs <- T.mapM loadFromLoader loaders
  let env = stripUnits . addSerials $ seqs
  return . runIdentity $ runReaderT rdr env

-- | Loads transactions and prices from the given file on disk.
open
  :: String
  -> Clatcher Scroll (Seq Loader) rpt ()
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
  -> Clatcher Scroll (Seq Loader) rpt ()
reuse preload = modify f
  where
    f x = x { runLedger = Just runLedger' }
      where
        runLedger' = case runLedger x of
          Nothing -> (Seq.singleton (Preloaded preload), loadLedger)
          Just (rnr, _) -> (rnr |> Preloaded preload, loadLedger)

-- ## General

