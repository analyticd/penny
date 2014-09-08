{-# LANGUAGE NoImplicitPrelude #-}
-- | Positive numbers.  This module exports everything but the
-- constructor.
module Penny.Pos
  ( T
  -- * Conversions
  , toInteger
  , fromInteger
  , fromNonNeg
  , fromNovem
  , fromDecem

  -- * Arithmetic
  , add
  , addNonNeg
  , mult
  , pred
  , succ

  -- * Constants
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

import Prelude hiding
  ( toInteger
  , fromInteger
  , pred
  , succ
  )
import qualified Prelude
import qualified Penny.NonNeg.Type as NonNeg
import Penny.Pos.Internal
import Deka.Native.Abstract

fromInteger :: Integer -> Maybe T
fromInteger i
  | i < 1 = Nothing
  | otherwise = Just . T $ i

fromNonNeg :: NonNeg.T -> Maybe T
fromNonNeg x
  | xi > 0 = Just . T $ xi
  | otherwise = Nothing
  where
    xi = NonNeg.toInteger x

fromNovem :: Novem -> T
fromNovem = T . novemToInt

fromDecem :: Decem -> Maybe T
fromDecem d = case d of
  Nonem n -> Just $ fromNovem n
  _ -> Nothing

add :: T -> T -> T
add (T x) (T y) = T $ x + y

addNonNeg :: T -> NonNeg.T -> T
addNonNeg (T x) y = T $ x + NonNeg.toInteger y

mult :: T -> T -> T
mult (T x) (T y) = T $ x * y

pred :: T -> Maybe T
pred (T x)
  | x == 1 = Nothing
  | otherwise = Just . T $ Prelude.pred x

succ :: T -> T
succ (T x) = T $ Prelude.succ x

zero :: T
zero = T 0

one :: T
one = T 1

two :: T
two = T 2

three :: T
three = T 3

four :: T
four = T 4

five :: T
five = T 5

six :: T
six = T 6

seven :: T
seven = T 7

eight :: T
eight = T 8

nine :: T
nine = T 9

ten :: T
ten = T 10
