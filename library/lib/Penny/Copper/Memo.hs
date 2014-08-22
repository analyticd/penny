{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Memo
  ( MemoLine
  , unMemoLine
  , toMemoLine
  ) where

import Control.Applicative hiding (many)
import qualified Data.Text as X
import Text.Parsec
import Penny.Copper.Render
import Data.Monoid

-- | A line in a memo.  Has no newlines.  Can be completely empty.
newtype MemoLine = MemoLine { unMemoLine :: X.Text }
  deriving (Eq, Ord, Show)

toMemoLine :: X.Text -> Maybe MemoLine
toMemoLine x
  | X.all (/= '\n') x = Just (MemoLine x)
  | otherwise = Nothing

instance Renderable MemoLine where
  render (MemoLine x) = ";" <> x <> "\n"

  parser =
    char ';'
    *> (fmap (MemoLine . X.pack) (many (satisfy (/= '\n'))))
    <* char '\n'
