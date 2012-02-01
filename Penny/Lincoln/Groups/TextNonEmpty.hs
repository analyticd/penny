module Penny.Lincoln.Groups.TextNonEmpty where

import Data.Text ( Text )

data TextNonEmpty = TextNonEmpty { first :: Char
                                 , rest :: Text }
                    deriving (Eq, Ord, Show)



  
