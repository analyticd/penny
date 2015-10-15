{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Streams of textual data that can be printed to terminals or sent
-- to files.
module Penny.Stream where

import qualified Control.Concurrent.Async as Async
import Control.Exception (bracketOnError)
import Control.Lens (view, makeLenses, runLens)
import Control.Monad.Managed.Safe (Managed, managed, runManaged)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Pipes (Consumer, Pipe, await, yield, (>->), liftIO, runEffect, each)
import Pipes.Cliff
  (pipeInput, NonPipe(..), waitForProcess, terminateProcess, procSpec)
import Pipes.Prelude (drain)
import Pipes.Safe (SafeT, runSafeT)
import Rainbow (Chunk)
import qualified System.IO as IO

import Penny.Colorize

type Stream = Managed (Consumer (Chunk Text) (SafeT IO) ())

chunkConverter
  :: Monad m
  => ((Chunk Text) -> [ByteString] -> [ByteString])
  -> Pipe (Chunk Text) ByteString m a
chunkConverter f = do
  ck <- await
  let bss = f ck []
  mapM_ yield bss
  chunkConverter f

-- | A Stream that discards all data.
devNull :: Stream
devNull = managed f
  where
    f cont = cont drain

-- | A Stream that creates a process and pipes data into the process
-- through its standard input.

streamToStdin
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Arguments
  -> ((Chunk Text) -> [ByteString] -> [ByteString])
  -- ^ Chunk converter
  -> Stream
streamToStdin name args conv = managed f
  where
    f callback = bracketOnError acq rel use
      where
        acq = pipeInput Inherit Inherit (procSpec name args)
        rel (_, handle) = terminateProcess handle
        use (strm, handle) = do
          r <- callback (chunkConverter conv >-> strm >> return ())
          waitForProcess handle
          return r


-- | Creates a stream that accepts input and sends it to a file.
streamToFile
  :: Bool
  -- ^ If True, append; otherwise, overwrite.
  -> String
  -- ^ Filename
  -> (Chunk Text -> [ByteString] -> [ByteString])
  -> Stream
streamToFile apnd fn conv = do
  h <- managed $ IO.withFile fn (if apnd then IO.AppendMode else IO.WriteMode)
  let toFile = do
        ck <- await
        liftIO $ mapM_ (BS.hPut h) (conv ck [])
        toFile
  return toFile

-- | Given a sequence of chunks and a 'Stream', forks a thread to the
-- background and feeds data to the stream.  The use of 'Managed'
-- ensures that the background thread is terminated if an exception is
-- thrown.  'runStream' waits for the background thread to terminate
-- normally (what this means depends on the underlying 'Stream').
runStream
  :: Seq (Chunk Text)
  -> Stream
  -> Managed ()
runStream sq str = do
  thread <- managed $ Async.withAsync acq
  liftIO $ Async.wait thread
  where
    acq = runManaged $ do
      csmr <- str
      liftIO $ runSafeT (runEffect (each sq >-> csmr))

-- | Runs a series of 'Stream's.

runStreams
  :: Seq (Chunk Text)
  -- ^ Feed in these 'Chunk's
  -> Seq Stream
  -> IO ()
runStreams cks = runManaged . fmap (const ()) . traverse (runStream cks)

class Streamable a where
  stream :: a -> Stream

-- | Record for data to create a process that reads from its standard input.
data StdinProcess = StdinProcess
  { _programName :: String
  , _programArgs :: [String]
  }

makeLenses ''StdinProcess

instance Monoid StdinProcess where
  mempty = StdinProcess mempty mempty
  mappend (StdinProcess x0 x1) (StdinProcess y0 y1)
    = StdinProcess (x0 <> y0) (x1 <> y1)


instance Streamable (Colorable StdinProcess) where
  stream (Colorable clrs stp) = do
    chooser <- liftIO tputColors
    let clrzr = colorizer (view (runLens chooser) clrs)
    streamToStdin (_programName stp) (_programArgs stp) clrzr


-- | Data to create a sink that puts data into a file.
data FileSink = FileSink
  { _sinkFilename :: String
  , _appendToFile :: Bool
  }

instance Monoid FileSink where
  mempty = FileSink mempty False
  mappend (FileSink x0 x1) (FileSink y0 y1)
    = FileSink (x0 <> y0) (x1 && y1)

makeLenses ''FileSink

instance Streamable (Colorable FileSink) where
  stream (Colorable clrs (FileSink fn apnd)) = do
    chooser <- liftIO tputColors
    let clrzr = colorizer (view (runLens chooser) clrs)
    streamToFile apnd fn clrzr

-- | Creates a stream that sends output to @less@.  By default,
-- the number of colors is the maximum number allowed by the terminal.
toLess :: Colorable StdinProcess
toLess = Colorable clrs (StdinProcess "less" ["-R"])
  where
    clrs = ChooseColors
      { _canShow0 = HowManyColors False False
      , _canShow8 = HowManyColors True False
      , _canShow256 = HowManyColors True True
      }

-- | Creates a stream that sends output to a file.
-- By default, no colors are used under any circumstance, and any
-- existing file is replaced.
toFile
  :: String
  -- ^ Filename
  -> Colorable FileSink
toFile fn = Colorable clrs (FileSink fn False)
  where
    clrs = ChooseColors
      { _canShow0 = HowManyColors False False
      , _canShow8 = HowManyColors False False
      , _canShow256 = HowManyColors False False
      }
