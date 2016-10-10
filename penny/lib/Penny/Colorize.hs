{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Penny.Colorize where

import Control.Exception (catch, IOException)
import Control.Lens
import Data.Monoid
import Data.Text (Text)
import Rainbow
import System.Process
import Text.Read (readMaybe)

-- | How many colors to show for a particular terminal.
data HowManyColors = HowManyColors
  { _showAnyColors :: Bool
  -- ^ If True, show colors.  How many colors to show depends on the
  -- value of '_show256Colors'.

  , _show256Colors :: Bool
  -- ^ If True and '_showAnyColors' is True, show 256 colors.  If
  -- False and '_showAnyColors' is True, show 8 colors.  If
  -- '_showAnyColors' is False, no colors are shown regardless of the
  -- value of '_show256Colors'.
  } deriving (Eq, Show, Ord)

makeLenses ''HowManyColors

noColors :: HowManyColors
noColors = HowManyColors False False

colors8 :: HowManyColors
colors8 = HowManyColors True False

colors256 :: HowManyColors
colors256 = HowManyColors True True

colorizer
  :: HowManyColors
  -> Chunk Text
  -> [ByteString]
  -> [ByteString]
colorizer (HowManyColors anyC c256)
  | not anyC = toByteStringsColors0
  | not c256 = toByteStringsColors8
  | otherwise = toByteStringsColors256

-- | With 'mempty', both fields are 'True'.  'mappend' runs '&&' on
-- both fields.
instance Monoid HowManyColors where
  mempty = HowManyColors True True
  mappend (HowManyColors x1 x2) (HowManyColors y1 y2)
    = HowManyColors (x1 && y1) (x2 && y2)

-- | How many colors to show, for various terminals.
data ChooseColors = ChooseColors
  { _canShow0 :: HowManyColors
  -- ^ Show this many colors when @tput@ indicates that the terminal
  -- can show no colors, or when @tput@ fails.

  , _canShow8 :: HowManyColors
  -- ^ Show this many colors when @tput@ indicates that the terminal
  -- can show at least 8, but less than 256, colors.

  , _canShow256 :: HowManyColors
  -- ^ Show this many colors when @tput@ indicates that the terminal
  -- can show 256 colors.
  } deriving (Eq, Show, Ord)

makeLenses ''ChooseColors

alwaysNoColors :: ChooseColors
alwaysNoColors = ChooseColors noColors noColors noColors

autoColors :: ChooseColors
autoColors = ChooseColors noColors colors8 colors256

maxColors :: ChooseColors
maxColors = ChooseColors colors256 colors256 colors256

tputColors :: IO (ReifiedLens' ChooseColors HowManyColors)
tputColors = catch getLens handle
  where
    handle :: IOException -> IO (ReifiedLens' ChooseColors HowManyColors)
    handle _ = return $ Lens canShow0
    getLens = do
      str <- readProcess "tput" ["colors"] ""
      return $ case readMaybe (init str) of
        Nothing -> Lens canShow0
        Just i
          | i < (8 :: Int) -> Lens canShow0
          | i < 256 -> Lens canShow8
          | otherwise -> Lens canShow256

-- | Uses the 'Monoid' instance of 'HowManyColors'.
instance Monoid ChooseColors where
  mempty = ChooseColors mempty mempty mempty
  mappend (ChooseColors x0 x1 x2) (ChooseColors y0 y1 y2)
    = ChooseColors (x0 <> y0) (x1 <> y1) (x2 <> y2)

getColorizer
  :: ChooseColors
  -> IO (Chunk Text -> [ByteString] -> [ByteString])
getColorizer cc = do
  reified <- tputColors
  return $ colorizer (view (runLens reified) cc)

