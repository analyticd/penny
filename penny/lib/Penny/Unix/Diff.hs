{-# LANGUAGE OverloadedStrings #-}
module Penny.Unix.Diff
  ( Patch
  , viewDiff
  , destFile
  , newText
  , diff
  , patch
  , colordiff
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as XIO
import qualified Data.Text as X
import qualified Rainbow as R
import qualified Turtle
import qualified Turtle.Bytes as Bytes

-- | Options for @diff@.  Assumes we're using GNU diff.
diffOpts :: [Text]
diffOpts = ["--unified"]

-- | Runs GNU diff.  The from file is read from disk.  The to file is
-- supplied from Haskell via a 'Turtle.Shell' 'Turtle.Line', which is
-- then supplied to @diff@ on its standard input.
--
-- The output is strict 'Text' from @diff@.  diff always returns a
-- non-zero exit code if the files differ, so the exit code is
-- discarded.
runDiff
  :: MonadIO io
  => FilePath
  -- ^ From file
  -> Text
  -- ^ To file
  -> io Text
  -- ^ Output from @diff@
runDiff from to = fmap snd runProcess
  where
    args = diffOpts <> [ X.pack from, "-"]
    runProcess = Turtle.procStrict "diff" args
      (Turtle.select . Turtle.textToLines $ to)

data Patch = Patch
  { viewDiff :: Text
  -- ^ Output from 'diff'
  , destFile :: FilePath
  -- ^ Destination file that will be modified
  , newText :: Text
  -- ^ Text for the new file
  }

-- | Takes a @diff@ from the given old file to the new file.
diff
  :: MonadIO io
  => FilePath
  -- ^ From file
  -> Text
  -- ^ To file
  -> io Patch
diff from to = fmap makePatch (runDiff from to)
  where
    makePatch result = Patch result from to

-- | Applies a patch.  Does not actually run the @patch@ program;
-- instead, merely overwrites the destination file with the new text.
patch
  :: MonadIO io
  => Patch
  -> io ()
patch (Patch _ dest txt)
  = liftIO $ XIO.writeFile dest txt

less
  :: MonadIO io
  => Turtle.Shell BS.ByteString
  -> io ()
less = Bytes.procs "less" lessOpts
  where
    lessOpts = ["--RAW-CONTROL-CHARS", "--chop-long-lines"]

colorizeDiffLine
  :: Turtle.Line
  -> [ByteString]
colorizeDiffLine line
  = R.chunksToByteStrings R.toByteStringsColors8 [chk, nl]
  where
    nl = R.chunk . X.singleton $ '\n'
    txt = Turtle.lineToText line
    chk = R.chunk txt & changeColor
    changeColor = case X.uncons txt of
      Nothing -> id
      Just (x, _)
        | x == '-' -> R.fore R.red
        | x == '+' -> R.fore R.green
        | otherwise -> id

-- | Colorizes output from @diff@ and sends the output to @less@.
colordiff
  :: MonadIO io
  => Patch
  -> io ()
colordiff
  = less
  . Turtle.select
  . concat
  . fmap colorizeDiffLine
  . Turtle.textToLines
  . viewDiff
