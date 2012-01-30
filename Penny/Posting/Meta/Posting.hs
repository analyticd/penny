module Penny.Posting.Meta.Posting where

newtype Line = Line { unLine :: Int }
               deriving Show

newtype Column = Column { unColumn :: Int }
                 deriving Show

data Meta = Meta { line :: Line
                 , column :: Column
                 , format ::
