module Penny.Copper.Tree.Comment
  ( Comment(..)
  , CommentChar
  , unCommentChar
  , commentChar
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

data Comment = Comment Octothorpe (Seq CommentChar) Newline
  deriving (Eq, Ord, Show)

data CommentChar = CommentChar { unCommentChar :: Char }
  deriving (Eq, Ord, Show)

commentChar :: Char -> Maybe CommentChar
commentChar c
  | c /= '\n' = Just $ CommentChar c
  | otherwise = Nothing
