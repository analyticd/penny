module Penny.Copper.Ast where

import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Penny.Copper.Classes
import Penny.Copper.Intervals

data Located a = Located a LineColPos
  deriving Show

instance Functor Located where
  fmap f (Located a l) = Located (f a) l

rangeToParser :: Intervals Char -> Parser Char
rangeToParser i = case intervalsToTuples i of
  [] -> error "rangeToParser: empty interval"
  xs -> foldl1 (<|>) . map pRange $ xs

commentChar :: Intervals CommentChar
commentChar = fmap CommentChar $ included [range minBound maxBound]
  `remove` alone '\n'

newtype CommentChar = CommentChar Char
  deriving (Eq, Ord, Show)

