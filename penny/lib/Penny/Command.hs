{-# LANGUAGE RankNTypes #-}

module Penny.Command where

import Penny.Amount
import Penny.Commodity
import Penny.Clatch
import Penny.Clatcher (Clatcher, Report)
import qualified Penny.Clatcher as Clatcher
import Penny.Colors
import qualified Penny.Columns as Columns
import Penny.Converter
import Penny.Cursor
import Penny.Decimal
import Penny.NonNegative
import Penny.Price
import Penny.Stream
import Penny.TransactionBare

import Control.Lens (set, Getter, view, to)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

-- | Parses an unsigned decimal.  Applies 'error' if a value cannot be parsed.
unsigned :: Text -> DecUnsigned
unsigned = undefined

-- # Commands

convert
  :: Commodity
  -- ^ Convert from this commodity
  -> Commodity
  -- ^ Convert to this commodity
  -> Text
  -- ^ One unit of the from commodity equals this many of the to
  -- commodity.  Enter here a value that can be parsed into a zero or
  -- positive quantity.  If the value does not parse, 'error' will be
  -- applied with a short but slightly helpful error message.
  -> Clatcher
convert fromCy toCy factorTxt = set Clatcher.converter cv mempty
  where
    cv = Converter fn
    fn (Amount oldCy oldQty)
      | oldCy /= fromCy = Nothing
      | otherwise = Just $ Amount toCy (oldQty * factor)
    factor = fmap c'Integer'NonNegative . unsigned $ factorTxt

sieve
  :: (Converted (Maybe Cursor) () -> Bool)
  -> Clatcher
sieve f = set Clatcher.sieve f mempty

sort :: Ord a => (Prefilt (Maybe Cursor) () -> a) -> Clatcher
sort f = set Clatcher.sort (comparing f) mempty

screen
  :: (Totaled (Maybe Cursor) () -> Bool)
  -> Clatcher
screen f = set Clatcher.screen f mempty


-- Output

output :: Stream -> Clatcher
output s = set Clatcher.output (Seq.singleton s) mempty

less :: Clatcher
less = output $ stream toLess

saveAs :: String -> Clatcher
saveAs = output . stream . toFile

colors :: Colors -> Clatcher
colors c = set Clatcher.colors c mempty

report :: Report -> Clatcher
report r = set Clatcher.report r mempty

-- Load

open :: String -> Clatcher
open str = set Clatcher.load
  (Seq.singleton (Clatcher.loadCopper str)) mempty

preload
  :: (Seq Price, Seq (TransactionBare (Maybe Cursor)))
  -> Clatcher
preload pair = set Clatcher.load (Seq.singleton (return pair)) mempty

penny :: Clatcher -> IO ()
penny = fmap (const ()) . Clatcher.runClatcher

-- | A point-free version of '&&'.

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
l &&& r = \a -> l a && r a
infixr 3 &&&

-- | A point-free version of '||'.
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
l ||| r = \a -> l a || r a
infixr 2 |||

-- # Standard reports

-- | A standard columns report, with the following columns:
--
-- * date
-- * flag
-- * number
-- * payee
-- * posting troika
-- * balance troikas
