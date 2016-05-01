{-# LANGUAGE RankNTypes #-}

module Penny.Command where

import Penny.Amount
import Penny.Commodity
import Penny.Clatch
import Penny.Clatcher (Clatcher)
import qualified Penny.Clatcher as Clatcher
import Penny.Colors
import Penny.Columns (Colable, Columns)
import qualified Penny.Columns as Columns
import Penny.Converter
import Penny.Decimal
import Penny.NonNegative
import Penny.Ord
import Penny.Report
import Penny.Stream

import Control.Lens (set, Getter, view, to)
import qualified Data.Sequence as Seq
import Data.Text (Text)


-- | Parses an unsigned decimal.  Applies 'error' if a value cannot be parsed.
unsigned :: Text -> DecUnsigned
unsigned = undefined

-- # Commands

convert
  :: Monoid r
  => Commodity
  -- ^ Convert from this commodity
  -> Commodity
  -- ^ Convert to this commodity
  -> Text
  -- ^ One unit of the from commodity equals this many of the to
  -- commodity.  Enter here a value that can be parsed into a zero or
  -- positive quantity.  If the value does not parse, 'error' will be
  -- applied with a short but slightly helpful error message.
  -> Clatcher r l
convert fromCy toCy factorTxt = set Clatcher.converter cv mempty
  where
    cv = Converter fn
    fn (Amount oldCy oldQty)
      | oldCy /= fromCy = Nothing
      | otherwise = Just $ Amount toCy (oldQty * factor)
    factor = fmap c'Integer'NonNegative . unsigned $ factorTxt

sieve
  :: Getter (Converted ()) Bool
  -> Clatcher r l
sieve f = set Clatcher.sieve (view f) mempty

sort :: Ord a => Getter (Prefilt ()) a -> Clatcher r l
sort f = set Clatcher.sort (comparing f) mempty

screen
  :: Getter (Totaled ()) Bool
  -> Clatcher r l
screen f = set Clatcher.screen (view f) mempty


-- Output

output :: Stream -> Clatcher r l
output s = set Clatcher.output (Seq.singleton s) mempty

less :: Clatcher r l
less = output $ stream toLess

saveAs :: String -> Clatcher r l
saveAs = output . stream . toFile

colors :: Colors -> Clatcher r l
colors c = set Clatcher.colors c mempty

-- Report

report :: r -> Clatcher r l
report s = set Clatcher.report (Seq.singleton s) mempty

column :: Colable a => Getter Clatch a -> Clatcher Columns l
column f = set Clatcher.report (Seq.singleton $ Columns.column f) mempty

-- Load

preload :: String -> IO (Clatcher r Clatcher.LoadScroll)
preload = fmap make . Clatcher.preload
  where
    make scroll = set Clatcher.load (Seq.singleton scroll) mempty

open :: String -> Clatcher r Clatcher.LoadScroll
open str = set Clatcher.load (Seq.singleton (Clatcher.open str)) mempty

penny :: (Report r, Clatcher.Loader l) => Clatcher r l -> IO ()
penny = fmap (const ()) . Clatcher.clatcher

-- Combinators

-- | A point-free, lens-friendly version of '&&'.
--
-- >>> :{
-- >>> penny $ open "myfile" <> register <>
-- >>> sieve (account . to (== ["Assets", "Checking"])
-- >>>        &&& (flag . to (== "R")))
-- >>> :}

(&&&) :: Getter a Bool -> Getter a Bool -> Getter a Bool
l &&& r = to $ \a -> view l a && view r a
infixr 3 &&&

-- | A point-free, lens-friendly version of '||'.
--
-- >>> :{
-- >>> penny $ open "myfile" <> register <>
-- >>> sieve (account . to (["Assets"] `isPrefixOf`)
-- >>>        ||| account . to (["Liabilities"] `isPrefixOf`))
-- >>> :}

(|||) :: Getter a Bool -> Getter a Bool -> Getter a Bool
l ||| r = to $ \a -> view l a || view r a
infixr 2 |||
