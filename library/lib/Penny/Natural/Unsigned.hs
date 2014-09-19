-- | Integral unsigned numbers; includes zero.
module Penny.Natural.Unsigned
  ( T
  -- * Conversions
  , toInteger
  , fromInteger
  , fromNonZero
  , fromNovem
  , fromDecem

  -- * Arithmetic
  , add
  , addNonZero
  , mult
  , monus
  , subt
  , div
  , exp
  , succ
  , pred

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
  , length
  ) where

import qualified Data.Foldable as F
import Deka.Native.Abstract
import Prelude hiding
  ( toInteger
  , fromInteger
  , exp
  , succ
  , pred
  , div
  , length
  )
import qualified Prelude
import qualified Penny.Natural.NonZero as NonZero
import Penny.Natural.Unsigned.Internal

fromInteger :: Integer -> Maybe T
fromInteger i
  | i >= 0 = Just . T $ i
  | otherwise = Nothing

fromNonZero :: NonZero.T -> T
fromNonZero = T . NonZero.toInteger

add :: T -> T -> T
add (T x) (T y) = T $ x + y

addNonZero :: T -> NonZero.T -> T
addNonZero (T x) p = T $ x + NonZero.toInteger p

mult :: T -> T -> T
mult (T x) (T y) = T $ x * y

monus :: T -> T -> T
monus (T x) (T y) = T . max 0 $ x - y

subt :: T -> T -> Maybe T
subt (T x) (T y)
  | y > x = Nothing
  | otherwise = Just . T $ x - y

exp :: T -> T -> T
exp (T x) (T y) = T $ x ^ y

succ :: T -> T
succ (T x) = T $ Prelude.succ x

pred :: T -> Maybe T
pred (T x)
  | x < 0 = Nothing
  | otherwise = Just . T . Prelude.pred $ x

div :: T -> NonZero.T -> (T, T)
div (T x) py = (T q, T r)
  where
    (q, r) = x `divMod` (NonZero.toInteger py)

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

length :: F.Foldable f => f a -> T
length = T . fromIntegral . Prelude.length . F.toList

fromNovem :: Novem -> T
fromNovem = T . novemToInt

fromDecem :: Decem -> T
fromDecem = T . decemToInt
