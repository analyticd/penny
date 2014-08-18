module Penny.Copper.Commodity.NonCurrency
  ( NonCurrency
  , unNonCurrency
  , commodityToNonCurrency
  ) where

import Control.Monad
import Penny.Common
import Penny.Copper.Render
import Text.Parsec
import Data.Char
import qualified Data.Text as X

-- | A 'Commodity' where the first character is a letter and each
-- subsequent characer is not a space or a newline.  This name cannot
-- be empty.
newtype NonCurrency = NonCurrency { unNonCurrency :: Commodity }
  deriving (Eq, Ord, Show)

commodityToNonCurrency :: Commodity -> Maybe NonCurrency
commodityToNonCurrency (Commodity x) = case X.uncons x of
  Nothing -> Nothing
  Just (c, xs) -> do
    guard $ isLetter c
    guard $ X.all (not . banned) xs
    return . NonCurrency . Commodity $ x

banned :: Char -> Bool
banned c =
  c == ' '
  || c == '\n'

instance Renderable NonCurrency where
  render (NonCurrency (Commodity x)) = x
  parse =
    fmap (NonCurrency . Commodity . X.pack) $
      (liftM2 (:) letter (many (satisfy (not . banned))))
