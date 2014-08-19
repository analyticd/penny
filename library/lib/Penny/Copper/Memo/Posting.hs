{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Memo.Posting
  ( PostingMemo
  , unPostingMemo
  , memoToPostingMemo
  ) where

import Control.Applicative hiding (many)
import Penny.Common
import qualified Data.Text as X
import qualified Data.Foldable as F
import Text.Parsec
import Penny.Copper.Render
import Data.Monoid
import Data.Sequence

-- | A memo accompanying a posting.  Each item in the 'Memo' has no
-- newlines.  Can be completely empty.
newtype PostingMemo = PostingMemo { unPostingMemo :: Memo }
  deriving (Eq, Ord, Show)

memoToPostingMemo :: Memo -> Maybe PostingMemo
memoToPostingMemo (Memo xs)
  | F.all (X.all (/= '\n')) xs = Just (PostingMemo (Memo xs))
  | otherwise = Nothing

instance Renderable PostingMemo where
  render (PostingMemo (Memo xs)) =
    X.concat . F.toList . fmap renderLine $ xs
    where
      renderLine x = ";" <> x <> "\n"

  parse = fmap (PostingMemo . Memo . fromList) $ many postingLine
    where
      postingLine =
        fmap X.pack
        $ char ';' *> many (satisfy (/= '\n')) <* char '\n'
