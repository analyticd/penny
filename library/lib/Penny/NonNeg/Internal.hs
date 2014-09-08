{-# OPTIONS_HADDOCK not-home #-}
module Penny.NonNeg.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

