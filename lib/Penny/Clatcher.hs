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
  , preFilter :: c (PreFilter l)
  , sorter :: c (Seq (Filtereds l -> l (Filtereds l)))
  , postFilter :: c (PostFilter l)
  , convertPreFilter :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , convertPostFilter :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , convertMain :: c (IO (Chunk -> [ByteString] -> [ByteString]))
  , streamPreFilter :: c (IO Stream)
  , streamPostFilter :: c (IO Stream)
  , streamMain :: c (IO Stream)
  , report :: c (rpt, rpt -> Reporter l)
  , runLedger :: c (ldr, ldr -> l AllChunks -> IO AllChunks)
  }

type Clatcher l ldr rpt
  = State (ClatchOptions Maybe l ldr rpt)

defaultClatchOptions :: Monad l => ClatchOptions Identity l ldr rpt
defaultClatchOptions = ClatchOptions
  { converter = Identity (const Nothing)
  , renderer = Identity (Right (Just Comma))
  , preFilter = Identity always
  , sorter = Identity Seq.empty
  , postFilter = Identity always
  , convertPreFilter = Identity byteStringMakerFromEnvironment
  , convertPostFilter = Identity byteStringMakerFromEnvironment
  , convertMain = Identity byteStringMakerFromEnvironment
  , streamPreFilter = Identity (return devNull)
  , streamPostFilter = Identity (return devNull)
  , streamMain = Identity runLess
  , report = Identity (undefined, \_ _ _ -> [])
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

emptyClatchOpts :: ClatchOptions Maybe l ldr rpt
emptyClatchOpts = ClatchOptions
  { converter = Nothing
  , renderer = Nothing
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

feeder
  :: ClatchOptions Identity l ldr rpt
  -> (ClatchOptions Identity l ldr rpt
      -> Identity (IO (Chunk -> [ByteString] -> [ByteString])))
  -> (ClatchOptions Identity l ldr rpt
      -> Identity (IO Stream))
  -> Seq Chunk
  -> IO a
  -> IO a
feeder opts getConv getStrm sq rest = do
  let strm = getStrm opts
  cvtr <- runIdentity $ getConv opts
  feedStream (runIdentity strm) cvtr sq rest

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
                    (runIdentity $ preFilter opts)
                    (runIdentity $ sorter opts)
                    (runIdentity $ postFilter opts)
      let smartRender (Amount cy qt)
            = c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
            $ repQtySmartly (runIdentity $ renderer opts)
                            (fmap (fmap snd) rndgs) cy qt
          (rptOpts, mkReport) = runIdentity $ report opts
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
  feeder opts convertPreFilter streamPreFilter msgsPre $
    feeder opts convertPostFilter streamPostFilter msgsPost $
    feeder opts convertMain streamMain cksRpt $
    return ()


-- | Runs the given command with options.
clatcher :: Ledger l => Clatcher l ldr rpt a -> IO ()
clatcher = clatcherWithDefaults defaultClatchOptions
-- # Available options

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

-- ## Reports
