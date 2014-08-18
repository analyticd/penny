{-# LANGUAGE OverloadedStrings #-}

module Penny.Copper.Memo.Transaction
  ( TransactionMemo
  , unTransactionMemo
  , memoToTransactionMemo
  ) where

import Control.Applicative hiding (many)
import Penny.Common
import qualified Data.Text as X
import qualified Data.Foldable as F
import Text.Parsec
import Penny.Copper.Render
import Data.Monoid
import Data.Sequence

-- | A memo accompanying a transaction.  Each item in the 'Memo' has
-- no newlines.  Can be completely empty.

newtype TransactionMemo = TransactionMemo { unTransactionMemo :: Memo }
  deriving (Eq, Ord, Show)

memoToTransactionMemo :: Memo -> Maybe TransactionMemo
memoToTransactionMemo (Memo xs)
  | F.all (X.all (/= '\n')) xs = Just (TransactionMemo (Memo xs))
  | otherwise = Nothing

instance Renderable TransactionMemo where
  render (TransactionMemo (Memo xs)) =
    X.concat . F.toList . fmap renderLine $ xs
    where
      renderLine x = ";" <> x <> "\n"

  parse = fmap (TransactionMemo . Memo . fromList) $ many transactionLine
    where
      transactionLine =
        fmap X.pack
        $ char ';' *> many (satisfy (/= '\n')) <* char '\n'
