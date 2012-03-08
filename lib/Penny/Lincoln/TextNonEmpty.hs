module Penny.Lincoln.TextNonEmpty where

import Data.Text ( Text, pack )

data TextNonEmpty = TextNonEmpty { first :: Char
                                 , rest :: Text }
                    deriving (Eq, Ord, Show)

unsafeTextNonEmpty :: String -> TextNonEmpty
unsafeTextNonEmpty s = TextNonEmpty (head s) (pack . tail $ s)
