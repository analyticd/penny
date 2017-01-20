{-# LANGUAGE OverloadedStrings #-}
-- | Functions useful when writing Unix command-line programs.
module Penny.Unix where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Turtle
import qualified Turtle.Bytes as Bytes


-- | Generic options for @less@.
lessOpts :: [Text]
lessOpts = ["--RAW-CONTROL-CHARS", "--chop-long-lines"]

-- | Runs @less@ in a subprocess with @lessOpts@.  Throws
-- 'Turtle.ProcFailed' for non-zero exit codes.
less
  :: MonadIO io
  => Turtle.Shell ByteString
  -> io ()
less = Bytes.procs "less" lessOpts

-- | Converts an 'IO' 'Text' to a 'Turtle.Shell' 'Turtle.Line'.
ioTextToShell :: IO Text -> Turtle.Shell Turtle.Line
ioTextToShell io = do
  text <- Turtle.liftIO io
  let lines = Turtle.textToLines text
  Turtle.select lines

-- | Converts a 'Text' to a 'Turtle.Shell' 'Turtle.Line'.
textToShell :: Text -> Turtle.Shell Turtle.Line
textToShell = Turtle.select . Turtle.textToLines
