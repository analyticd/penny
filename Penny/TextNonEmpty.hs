module Penny.TextNonEmpty where

import Data.Text ( Text )

data TextNonEmpty = TextNonEmpty { first :: Char
                                 , rest :: Text }

instance Eq TextNonEmpty where
  (==) (TextNonEmpty c1 t1) (TextNonEmpty c2 t2) =
    c1 == c2 && t1 == t2
