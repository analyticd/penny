module Penny.Tree.Currency
  ( T
  , toChar
  , fromChar
  , parser
  , toCommodity
  ) where

import Data.Char
import Penny.Tree.Parsec
import qualified Data.Text as X
import qualified Penny.Core.Commodity as Commodity

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | generalCategory c == CurrencySymbol = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "currency symbol" fromChar

toCommodity :: T -> Commodity.T
toCommodity (T c) = Commodity.T . X.singleton $ c
