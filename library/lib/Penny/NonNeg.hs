module Penny.NonNeg
  ( T
  -- * Conversions
  , toInteger
  , fromInteger
  , fromPos
  , fromNovem
  , fromDecem

  -- * Arithmetic
  , add
  , addPos
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
import qualified Penny.Pos as Pos
import Penny.NonNeg.Internal

fromInteger :: Integer -> Maybe T
fromInteger i
  | i >= 0 = Just . T $ i
  | otherwise = Nothing

fromPos :: Pos.T -> T
fromPos = T . Pos.toInteger

add :: T -> T -> T
add (T x) (T y) = T $ x + y

addPos :: T -> Pos.T -> T
addPos (T x) p = T $ x + Pos.toInteger p

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

div :: T -> Pos.T -> (T, T)
div (T x) py = (T q, T r)
  where
    (q, r) = x `divMod` (Pos.toInteger py)

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
