{-# LANGUAGE BangPatterns #-}
module Penny.ConvertFile where

import qualified Penny.Grammar as G
import qualified Data.Sequence as Seq
import Data.Sequence (viewr, ViewR(EmptyR, (:>)))

-- | Things that can be converted to an integer.
class ConvInt a where
  convInt :: Integral b => a -> b

instance ConvInt G.Zero where
  convInt (G.Zero _) = 0

instance ConvInt G.One where
  convInt (G.One _) = 1

instance ConvInt G.Two where
  convInt (G.Two _) = 2

instance ConvInt G.Three where
  convInt (G.Three _) = 3

instance ConvInt G.Four where
  convInt (G.Four _) = 4

instance ConvInt G.Five where
  convInt (G.Five _) = 5

instance ConvInt G.Six where
  convInt (G.Six _) = 6

instance ConvInt G.Seven where
  convInt (G.Seven _) = 7

instance ConvInt G.Eight where
  convInt (G.Eight _) = 8

instance ConvInt G.Nine where
  convInt (G.Nine _) = 9

instance ConvInt G.D1'9 where
  convInt x = case x of
    G.D1'9'One a -> convInt a
    G.D1'9'Two a -> convInt a
    G.D1'9'Three a -> convInt a
    G.D1'9'Four a -> convInt a
    G.D1'9'Five a -> convInt a
    G.D1'9'Six a -> convInt a
    G.D1'9'Seven a -> convInt a
    G.D1'9'Eight a -> convInt a
    G.D1'9'Nine a -> convInt a

instance ConvInt G.D0'9 where
  convInt x = case x of
    G.D0'9'Zero a -> convInt a
    G.D0'9'One a -> convInt a
    G.D0'9'Two a -> convInt a
    G.D0'9'Three a -> convInt a
    G.D0'9'Four a -> convInt a
    G.D0'9'Five a -> convInt a
    G.D0'9'Six a -> convInt a
    G.D0'9'Seven a -> convInt a
    G.D0'9'Eight a -> convInt a
    G.D0'9'Nine a -> convInt a

instance ConvInt G.D0'8 where
  convInt x = case x of
    G.D0'8'Zero a -> convInt a
    G.D0'8'One a -> convInt a
    G.D0'8'Two a -> convInt a
    G.D0'8'Three a -> convInt a
    G.D0'8'Four a -> convInt a
    G.D0'8'Five a -> convInt a
    G.D0'8'Six a -> convInt a
    G.D0'8'Seven a -> convInt a
    G.D0'8'Eight a -> convInt a

instance ConvInt G.D0'1 where
  convInt x = case x of
    G.D0'1'Zero a -> convInt a
    G.D0'1'One a -> convInt a

instance ConvInt G.D0'2 where
  convInt x = case x of
    G.D0'2'Zero a -> convInt a
    G.D0'2'One a -> convInt a
    G.D0'2'Two a -> convInt a

instance ConvInt G.D0'3 where
  convInt x = case x of
    G.D0'3'Zero a -> convInt a
    G.D0'3'One a -> convInt a
    G.D0'3'Two a -> convInt a
    G.D0'3'Three a -> convInt a

instance ConvInt G.D0'5 where
  convInt x = case x of
    G.D0'5'Zero a -> convInt a
    G.D0'5'One a -> convInt a
    G.D0'5'Two a -> convInt a
    G.D0'5'Three a -> convInt a
    G.D0'5'Four a -> convInt a
    G.D0'5'Five a -> convInt a

instance ConvInt G.D0'9'Seq where
  convInt (G.D0'9'Seq sqnce) = go (0 :: Int) 0 sqnce
    where
      go !i !acc sq = case viewr sq of
        EmptyR -> acc
        xs :> x -> go (i + 1) (acc + convInt x * 10 ^ i) xs
