{-# OPTIONS_HADDOCK not-home #-}
module Penny.NonZero.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

