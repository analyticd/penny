module Penny.Lincoln.TextNonEmpty (
  TextNonEmpty (TextNonEmpty, first, rest),
  unsafeTextNonEmpty,
  any, all) where

import Prelude hiding (any, all)
import Data.Text ( Text, pack )
import qualified Data.Text as X

data TextNonEmpty = TextNonEmpty { first :: Char
                                 , rest :: Text }
                    deriving (Eq, Ord, Show)

unsafeTextNonEmpty :: String -> TextNonEmpty
unsafeTextNonEmpty s = TextNonEmpty (head s) (pack . tail $ s)

any :: (Char -> Bool) -> TextNonEmpty -> Bool
any f (TextNonEmpty c ts) = f c || X.any f ts

all :: (Char -> Bool) -> TextNonEmpty -> Bool
all f (TextNonEmpty c ts) = f c && X.all f ts
