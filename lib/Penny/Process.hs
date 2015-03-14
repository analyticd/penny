{-# LANGUAGE RankNTypes #-}
module Penny.Process where

import Control.Monad.IO.Class
import System.Exit
import Pipes
import Pipes.Safe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Process
import System.IO
import qualified Control.Exception
import Control.Monad.Trans.Cont
import Control.Concurrent.Async

-- | Like 'Control.Exception.bracket' but also returns the result of
-- the releaser.
bracketWithRelease
  :: IO a
  -- ^ Acquire resource
  -> (a -> IO b)
  -- ^ Release resource
  -> (a -> IO c)
  -- ^ Use resource
  -> IO (c, b)
  -- ^ Returns the result of the use and the value from the releaser

bracketWithRelease before after thing =
  Control.Exception.mask $ \restore -> do
    a <- before
    r <- restore (thing a) `Control.Exception.onException` after a
    rel <- after a
    return (r, rel)

useProcess
  :: String
  -> [String]
  -> (Handle -> Handle -> Handle -> IO a)
  -> IO (a, ExitCode)
useProcess progName args use = do
  let cp = (proc progName args) { std_in = CreatePipe
                                , std_out = CreatePipe
                                , std_err = CreatePipe
                                }
      acq = createProcess cp
      rel (Just inp, Just outp, Just err, han) = do
        hClose inp
        hClose outp
        hClose err
        waitForProcess han
      rel _ = error "useProcess: error 1"
      runner (Just inp, Just outp, Just err, _) = use inp outp err
      runner _ = error "useProcess: error 2"
  bracketWithRelease acq rel runner

-- | I have no idea what this should be
bufSize :: Int
bufSize = 1024

produceFromHandle :: MonadIO m => Handle -> Producer' ByteString m ()
produceFromHandle h = liftIO (BS.hGetSome h bufSize) >>= go
  where
    go bs | BS.null bs = return ()
          | otherwise = yield bs >> produceFromHandle h

consumeToHandle :: MonadIO m => Handle -> Consumer' ByteString m ()
consumeToHandle h = do
  bs <- await
  liftIO $ BS.hPut h bs
  consumeToHandle h

runProcess
  :: (MonadIO xm, MonadIO ym, MonadIO zm)
  => RunProducer' ByteString xm ()
  -- ^ Stdin
  -> RunConsumer' ByteString ym ()
  -- ^ Stdout
  -> RunConsumer' ByteString zm ()
  -- ^ Stderr
  -> String
  -> [String]
  -> IO ExitCode
runProcess inpP outP errP progName args
  = fmap snd $ useProcess progName args user
  where
    getter han (RunProxy consmr runner) = runner $ runEffect $
      produceFromHandle han >-> consmr
    pusher han (RunProxy prodcr runner) = runner $ runEffect $
      prodcr >-> consumeToHandle han
    user inH outH errH = evalContT $ do
      aIn <- spawn (pusher inH inpP)
      aOut <- spawn (getter outH outP)
      aErr <- spawn (getter errH errP)
      lift $ wait aIn
      lift $ wait aOut
      lift $ wait aErr

spawn :: IO a -> ContT b IO (Async a)
spawn io = ContT (withAsync io)

data RunProxy a' a b' b m r
  = RunProxy (Proxy a' a b' b m r) (m r -> IO r)

type RunProducer' b m r = forall x' x. RunProxy x' x () b m r
type RunConsumer' a m r = forall y' y. RunProxy () a y' y m r
type RunProducer b = RunProxy X () () b
type RunConsumer a = Proxy () a () X
