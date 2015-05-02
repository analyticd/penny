{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Penny.Stream where

import Control.Lens
import Penny.Colorize
import Control.Exception (bracketOnError)
import Rainbow
import qualified Control.Concurrent.Async as Async
import Data.Monoid
import Data.Text (Text)
import Pipes
import Pipes.Prelude (tee, drain)
import Pipes.Safe (SafeT, runSafeT)
import Pipes.Cliff
  (pipeInput, NonPipe(..), waitForProcess, terminateProcess, procSpec)
import qualified System.IO as IO
import qualified Data.ByteString as BS
import Data.Sequence (Seq)


-- | An action that waits on a particular stream to finish.  This
-- action should block until the stream is done.
newtype Waiter = Waiter (IO ())

instance Monoid Waiter where
  mempty = Waiter (return ())
  mappend (Waiter x) (Waiter y) = Waiter $ Async.withAsync x $ \ax ->
    Async.withAsync y $ \ay -> do
      Async.wait ax
      Async.wait ay

-- | An action that terminates a stream right away.
newtype Terminator = Terminator (IO ())

instance Monoid Terminator where
  mempty = Terminator (return ())
  mappend (Terminator x) (Terminator y) = Terminator $ Async.withAsync x $ \ax ->
    Async.withAsync y $ \ay -> do
      Async.wait ax
      Async.wait ay


-- | A stream that accepts 'Chunk' 'Text', coupled with an action that
-- terminates the stream right away and an action that waits for the
-- stream to terminate normally.
data Stream = Stream (Consumer (Chunk Text) (SafeT IO) ()) Waiter Terminator

instance Monoid Stream where
  mempty = devNull
  mappend (Stream cx wx tx) (Stream cy wy ty)
    = Stream (tee cx >-> cy) (wx <> wy) (tx <> ty)

terminate :: Stream -> IO ()
terminate (Stream _ _ (Terminator t)) = t

wait :: Stream -> IO ()
wait (Stream _ (Waiter w) _) = w

devNull :: Stream
devNull = Stream drain (Waiter (return ())) (Terminator (return ()))

chunkConverter
  :: Monad m
  => ((Chunk Text) -> [ByteString] -> [ByteString])
  -> Pipe (Chunk Text) ByteString m a
chunkConverter f = do
  ck <- await
  let bss = f ck []
  mapM_ yield bss
  chunkConverter f

-- | Runs a stream that accepts on its standard input.
streamToStdin
  :: String
  -- ^ Program name
  -> [String]
  -- ^ Arguments
  -> ((Chunk Text) -> [ByteString] -> [ByteString])
  -- ^ Chunk converter
  -> IO Stream
streamToStdin name args conv = do
  (pipe, handle) <- pipeInput Inherit Inherit (procSpec name args)
  return (Stream (chunkConverter conv >-> pipe >> return ())
                 (Waiter (waitForProcess handle >> return ()))
                 (Terminator (terminateProcess handle)))

-- | Runs a stream that accepts input and sends it to a file.
streamToFile
  :: Bool
  -- ^ If True, append; otherwise, overwrite.
  -> String
  -- ^ Filename
  -> (Chunk Text -> [ByteString] -> [ByteString])
  -> IO Stream
streamToFile apnd fn conv = do
  h <- IO.openFile fn (if apnd then IO.AppendMode else IO.WriteMode)
  let toFile = do
        ck <- await
        liftIO $ mapM_ (BS.hPut h) (conv ck [])
        toFile
  return $ Stream toFile (Waiter (IO.hClose h))
    (Terminator (IO.hClose h))

feedStream
  :: IO Stream
  -> Seq (Chunk Text)
  -> IO b
  -> IO b
feedStream strm sq rest = withStream strm $ \str -> do
  let act = runSafeT $ runEffect $ Pipes.each sq >-> str
  bracketOnError (Async.async act) Async.cancel $ \asy -> do
    a <- rest
    Async.wait asy
    return a

-- | Runs a stream.  Under normal circumstances, waits for the
-- underlying process to stop running.  If an exception is thrown,
-- terminates the process immediately.
withStream
  :: IO Stream
  -> (Consumer (Chunk Text) (SafeT IO) () -> IO b)
  -> IO b
withStream acq useCsmr = bracketOnError acq terminate
  $ \str@(Stream csmr _ _) -> do
  r <- useCsmr csmr
  wait str
  return r


class Streamable a where
  toStream :: a -> IO Stream

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
  toStream (Colorable clrs stp) = do
    chooser <- tputColors
    let clrzr = colorizer (clrs ^. (runLens chooser))
    streamToStdin (_programName stp) (_programArgs stp) clrzr


-- | Data to create a sink that puts data into a file.
data FileSink = FileSink
  { _sinkFilename :: String
  , _appendToSink :: Bool
  }

instance Monoid FileSink where
  mempty = FileSink mempty False
  mappend (FileSink x0 x1) (FileSink y0 y1)
    = FileSink (x0 <> y0) (x1 && y1)

makeLenses ''FileSink

instance Streamable (Colorable FileSink) where
  toStream (Colorable clrs (FileSink fn apnd)) = do
    chooser <- tputColors
    let clrzr = colorizer (clrs ^. (runLens chooser))
    streamToFile apnd fn clrzr

-- | Creates a stream that, when you apply 'toStream' to the result,
-- sends output to @less@.  By default, the number of colors is the
-- maximum number allowed by the terminal.
toLess :: Colorable StdinProcess
toLess = Colorable clrs (StdinProcess "less" ["-R"])
  where
    clrs = ChooseColors
      { _canShow0 = HowManyColors False False
      , _canShow8 = HowManyColors True False
      , _canShow256 = HowManyColors True True
      }

-- | Creates a stream that, when you apply 'toStream' to the result,
-- sends output to a file.  By default, no colors are used under any
-- circumstance, and any existing file is replaced.
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
