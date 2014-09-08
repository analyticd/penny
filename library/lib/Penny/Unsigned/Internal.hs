{-# OPTIONS_HADDOCK not-home #-}
module Penny.Unsigned.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

