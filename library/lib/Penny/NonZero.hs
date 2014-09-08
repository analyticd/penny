{-# LANGUAGE NoImplicitPrelude #-}
-- | Positive numbers.  This module exports everything but the
-- constructor.
module Penny.NonZero
  ( T
  -- * Conversions
  , toInteger
  , fromInteger
  , fromUnsigned
  , fromNovem
  , fromDecem

  -- * Arithmetic
  , add
  , addUnsigned
  , mult
  , pred
  , succ

  -- * Constants
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
import qualified Penny.Unsigned.Type as Unsigned
import Penny.NonZero.Internal
import Deka.Native.Abstract

fromInteger :: Integer -> Maybe T
fromInteger i
  | i < 1 = Nothing
  | otherwise = Just . T $ i

fromUnsigned :: Unsigned.T -> Maybe T
fromUnsigned x
  | xi > 0 = Just . T $ xi
  | otherwise = Nothing
  where
    xi = Unsigned.toInteger x

fromNovem :: Novem -> T
fromNovem = T . novemToInt

fromDecem :: Decem -> Maybe T
fromDecem d = case d of
  Nonem n -> Just $ fromNovem n
  _ -> Nothing

add :: T -> T -> T
add (T x) (T y) = T $ x + y

addUnsigned :: T -> Unsigned.T -> T
addUnsigned (T x) y = T $ x + Unsigned.toInteger y

mult :: T -> T -> T
mult (T x) (T y) = T $ x * y

pred :: T -> Maybe T
pred (T x)
  | x == 1 = Nothing
  | otherwise = Just . T $ Prelude.pred x

succ :: T -> T
succ (T x) = T $ Prelude.succ x

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
