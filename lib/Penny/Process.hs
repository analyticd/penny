module Penny.Process where

import Control.Monad.IO.Class
import System.Exit
import Pipes
import Data.ByteString (ByteString)
import System.Process
import System.IO
import Control.Exception (onException)

data Chan m
  = Pusher (Producer ByteString m ())
  | Puller (Consumer ByteString m ())

useProcess
  :: String
  -> [String]
  -> ((Handle, Handle, Handle) -> IO ())
  -> IO ExitCode
useProcess progName args user = do
  let cp = (proc progName args) { std_in = CreatePipe
                                , std_out = CreatePipe
                                , std_err = CreatePipe
                                }
  (Just inp, Just outp, Just err, han) <- createProcess cp
  let release = waitForProcess han
  (user (inp, outp, err)) `onException` release
  release

runProcess
  :: MonadIO m
  => Producer ByteString m ()
  -- ^ Stdin
  -> Consumer ByteString m ()
  -- ^ Stdout
  -> Consumer ByteString m ()
  -- ^ Stderr
  -> String
  -> [String]
  -> m ExitCode
runProcess inpP outP errP progName args
  = liftIO $ useProcess progName args f
  where
    f (inH, outH, errH) = undefined
