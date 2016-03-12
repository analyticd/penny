{-# LANGUAGE BangPatterns #-}
module Penny.NonNegative
  ( NonNegative
  , c'Integer'NonNegative
  , c'Positive'NonNegative
  , length
  , next
  , prev
  , add
  , subt
  , zero
  , one
  , two
  , three
  , four
  , five
  , six
  , seven
  , eight
  , nine
  , ten
  ) where

import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)))
import Penny.NonNegative.Internal
import Prelude hiding (length)

length :: Seq a -> NonNegative
length = go zero
  where
    go !acc sq = case viewl sq of
      EmptyL -> acc
      _ :< xs -> go (next acc) xs
