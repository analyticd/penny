{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Memo.Posting
  ( PostingMemoLine
  , unPostingMemoLine
  , toPostingMemoLine
  ) where

import Control.Applicative hiding (many)
import qualified Data.Text as X
import Text.Parsec
import Penny.Copper.Render
import Data.Monoid

-- | A line in a memo accompanying a posting.  Has no newlines.  Can
-- be completely empty.
newtype PostingMemoLine = PostingMemoLine { unPostingMemoLine :: X.Text }
  deriving (Eq, Ord, Show)

toPostingMemoLine :: X.Text -> Maybe PostingMemoLine
toPostingMemoLine x
  | X.all (/= '\n') x = Just (PostingMemoLine x)
  | otherwise = Nothing

instance Renderable PostingMemoLine where
  render (PostingMemoLine x) = ";" <> x <> "\n"

  parse =
    char ';'
    *> (fmap (PostingMemoLine . X.pack) (many (satisfy (/= '\n'))))
    <* char '\n'
