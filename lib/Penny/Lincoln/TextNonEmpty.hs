module Penny.Lincoln.TextNonEmpty (
  TextNonEmpty,
  textNonEmpty,
  unsafeTextToNonEmpty,
  unsafeTextNonEmpty,
  any, all,
  toText) where

import Prelude hiding (any, all)
import Data.Text ( Text, pack )
import qualified Data.Text as X

newtype TextNonEmpty = TextNonEmpty Text
                       deriving (Eq, Ord, Show)

-- | Converts a string to a TextNonEmpty. You may want to consider
-- forcing the evaluation of the result of this function; otherwise,
-- the string you give to this function will remain on the heap until
-- it is evaluated. Strings take up much more space on the heap
-- than Texts (perhaps as much as ten times more.)
textNonEmpty :: Char -> String -> TextNonEmpty
textNonEmpty c cs = TextNonEmpty (pack (c:cs))

unsafeTextToNonEmpty :: Text -> TextNonEmpty
unsafeTextToNonEmpty t =
  if X.null t
  then error "unsafeTextToNonEmpty: Empty text"
  else TextNonEmpty t

unsafeTextNonEmpty :: String -> TextNonEmpty
unsafeTextNonEmpty s =
  if null s
  then error "unsafeTextNonEmpty: Empty string"
  else TextNonEmpty (pack s)

any :: (Char -> Bool) -> TextNonEmpty -> Bool
any f (TextNonEmpty ts) = X.any f ts

all :: (Char -> Bool) -> TextNonEmpty -> Bool
all f (TextNonEmpty ts) = X.all f ts

toText :: TextNonEmpty -> Text
toText (TextNonEmpty ts) = ts

