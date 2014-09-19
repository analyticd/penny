{-# OPTIONS_HADDOCK not-home #-}
module Penny.Natural.Unsigned.Internal where

newtype T = T { toInteger :: Integer }
  deriving (Eq, Ord, Show)

