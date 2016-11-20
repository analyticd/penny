{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Streams of textual data that can be printed to terminals or sent
-- to files.
--
-- If you are using bindings from this module,
--
-- /be sure to compile with the -threaded option/
--
-- otherwise your code might experience deadlocks.
module Penny.Stream where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exc
import Control.Monad.Managed.Safe (managed, runManaged)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Sequence (Seq)
import Data.Text (Text)
import Rainbow (Chunk)
import qualified System.Process as Process
import System.IO (Handle)
import qualified System.IO as IO

type Stream
  = (Chunk Text -> [ByteString] -> [ByteString])
  -> Seq (Chunk Text)
  -> IO ()

withProcess
  :: Process.CreateProcess
  -> ((Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle) -> IO r)
  -> IO r
withProcess cp = Exc.bracket acq rel
  where
    acq = Process.createProcess cp
    rel (inp, out, err, han)
      = close inp
      `Exc.finally` close out
      `Exc.finally` close err
      `Exc.finally` (Process.terminateProcess han)
      where
        close = maybe (return ()) IO.hClose

-- | A Stream that creates a process and pipes data into the process
-- through its standard input.  The 'Process.CreateProcess' is
-- modified to accept input on its standard input, but otherwise is
-- left as is.  Waits until the created process exits.
streamToStdin
  :: Process.CreateProcess
  -> Stream
streamToStdin cp conv sq = runManaged $ do
  let cp' = cp { Process.std_in = Process.CreatePipe }
  (Just inp, _, _, han) <- managed $ withProcess cp'
  let putChunks = mapM_ (BS.hPut inp) . foldr conv [] $ sq
  _ <- managed $ Async.withAsync putChunks
  _ <- liftIO $ Process.waitForProcess han
  return ()

-- | 'Process.CreateProcess' for @less@.
toLess :: Process.CreateProcess
toLess = (Process.proc "less" ["--RAW-CONTROL-CHARS", "--chop-long-lines"])
  { Process.std_in = Process.CreatePipe
  , Process.delegate_ctlc = True
  }

-- | Creates a stream that accepts input and sends it to a file.
streamToFile
  :: Bool
  -- ^ If True, append; otherwise, overwrite.
  -> String
  -- ^ Filename
  -> Stream
streamToFile apnd fn conv sq = runManaged $ do
  h <- managed $ IO.withFile fn (if apnd then IO.AppendMode else IO.WriteMode)
  liftIO . mapM_ (BS.hPut h) . foldr conv [] $ sq

