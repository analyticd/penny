module Penny.Copper.Commodity.Currency
  ( Currency
  , unCurrency
  , commodityToCurrency
  ) where

import Data.Char
import Penny.Common
import Text.Parsec
import Penny.Copper.Render
import qualified Data.Text as X

newtype Currency = Currency { unCurrency :: Commodity }
  deriving (Eq, Ord, Show)

commodityToCurrency :: Commodity -> Maybe Currency
commodityToCurrency (Commodity x) = case X.uncons x of
  Nothing -> Nothing
  Just (c, xs) -> case X.uncons xs of
    Nothing -> case generalCategory c of
      CurrencySymbol -> Just (Currency (Commodity x))
      _ -> Nothing
    Just _ -> Nothing


instance Renderable Currency where
  render (Currency (Commodity x)) = x
  parse = fmap (Currency . Commodity . X.singleton) $ satisfy f
    where
      f c = generalCategory c == CurrencySymbol
