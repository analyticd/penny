{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.NonZero.Internal where

newtype NonZero = NonZero { c'Integer'NonZero :: Integer }
  deriving (Eq, Ord, Show)

c'NonZero'Integer :: Integer -> Maybe NonZero
c'NonZero'Integer i
  | i == 0 = Nothing
  | otherwise = Just $ NonZero i
